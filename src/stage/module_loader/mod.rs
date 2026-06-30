//! Module loading: discover, parse, and validate every `.cb` file reachable from
//! an entry file via `use` statements.
//!
//! This is the front-end phase that runs before semantic analysis. It performs a
//! single-threaded worklist (BFS) over the import graph:
//!
//! 1. **Discovery** — parse the entry file, scan its `use` items, resolve each to
//!    a file on disk, and queue unseen files. Repeat until the worklist drains.
//!    Files are de-duplicated by canonical path so a diamond dependency is parsed
//!    once.
//! 2. **Resolution** — a `use a::b::c` path maps to a file by *longest file
//!    prefix wins*: probe the filesystem and take the longest leading run of
//!    segments that is a real `.cb` file; remaining segments are in-file
//!    selectors (v1 supports 0 or 1).
//! 3. **Graph + cycles** — record importer → dependency edges and reject cycles
//!    (Go-style), which also yields a usable processing order.
//! 4. **Visibility** — `use foo::bar::function` requires `function` to exist and
//!    be `pub` in module `bar`.
//!
//! Paths resolve relative to the importing file's directory. `std`/`core` are not
//! implemented yet.

use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};

use crate::error::{
    ErrorImportCycle, ErrorMessage, ErrorPrivateImport, ErrorUnresolvedImport, Errors, Report,
    Result, ScopedReport,
};
use crate::stage::Stage;
use crate::stage::lexer::token::Span;
use crate::stage::parser::ast::{self, Item, Visibility};

/// One parsed source file.
pub struct LoadedModule {
    /// Path as discovered, used for error reporting.
    pub path: PathBuf,
    /// File `math` for `math.cb`. Used as the namespace name for
    /// whole module imports `use foo::math` → `math::f()`.
    pub name: String,
    pub source: String,
    pub items: Vec<Item>,
}

impl LoadedModule {
    fn display(&self) -> String {
        self.path.display().to_string()
    }
}

/// All modules reachable from the entry file. `modules[0]` is the entry module.
pub struct LoadedProgram {
    pub modules: Vec<LoadedModule>,
}

pub struct ModuleLoader {
    unix_newlines: bool,
}

impl ModuleLoader {
    pub fn new(unix_newlines: bool) -> Self {
        Self { unix_newlines }
    }

    pub fn load(&self, entry: &Path) -> Result<LoadedProgram> {
        let mut errors: Vec<Box<dyn Report>> = Vec::new();
        let mut modules: Vec<LoadedModule> = Vec::new();
        // usize::MAX if it failed to parse
        let mut index_of: HashMap<PathBuf, usize> = HashMap::new();
        let mut raw_edges: Vec<(usize, PathBuf, Span)> = Vec::new();
        let mut pending_vis: Vec<PendingVis> = Vec::new();

        let mut queue: VecDeque<PathBuf> = VecDeque::new();
        queue.push_back(entry.to_path_buf());

        while let Some(path) = queue.pop_front() {
            let canonical = canonicalize_or(&path);
            if index_of.contains_key(&canonical) {
                continue;
            }

            let module = match self.parse_module(&path) {
                Ok(module) => module,
                Err(err) => {
                    errors.push(err);
                    index_of.insert(canonical, usize::MAX);
                    continue;
                }
            };

            let idx = modules.len();
            index_of.insert(canonical, idx);

            let base_dir = path
                .parent()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| PathBuf::from("."));

            for item in &module.items {
                let Item::Use(use_item) = item else {
                    continue;
                };
                let segments: Vec<String> =
                    use_item.path.iter().map(|t| t.lexeme.clone()).collect();
                if segments.is_empty() {
                    continue;
                }
                let span = use_span(use_item);
                let pretty = segments.join("::");

                match resolve_use_path(&base_dir, &segments) {
                    Some((dep_path, selectors)) if selectors.len() <= 1 => {
                        let dep_canonical = canonicalize_or(&dep_path);
                        raw_edges.push((idx, dep_canonical.clone(), span.clone()));
                        pending_vis.push(PendingVis {
                            importer_name: module.display(),
                            importer_source: module.source.clone(),
                            dep_canonical,
                            selector: selectors.into_iter().next(),
                            span,
                        });
                        queue.push_back(dep_path);
                    }
                    Some(_) => {
                        errors.push(Box::new(ScopedReport::new(
                            module.display(),
                            module.source.clone(),
                            Box::new(ErrorUnresolvedImport::new(
                                span,
                                pretty,
                                "nested module items are not supported yet",
                                #[cfg(feature = "debug")]
                                format!("{} {}:{}", file!(), line!(), column!()),
                            )),
                        )));
                    }
                    None => {
                        errors.push(Box::new(ScopedReport::new(
                            module.display(),
                            module.source.clone(),
                            Box::new(ErrorUnresolvedImport::new(
                                span,
                                pretty,
                                "no matching `.cb` file found relative to the importing file",
                                #[cfg(feature = "debug")]
                                format!("{} {}:{}", file!(), line!(), column!()),
                            )),
                        )));
                    }
                }
            }

            modules.push(module);
        }

        // NOTE: Visibility, item imports must name a public symbol in the dependency.
        for vis in &pending_vis {
            let Some(selector) = &vis.selector else {
                continue;
            };
            let Some(&dep_idx) = index_of.get(&vis.dep_canonical) else {
                continue;
            };
            if dep_idx == usize::MAX {
                continue;
            }
            let dep = &modules[dep_idx];
            match item_visibility(&dep.items, selector) {
                Some(Visibility::Public) => {}
                Some(Visibility::Private) => {
                    errors.push(Box::new(ScopedReport::new(
                        vis.importer_name.clone(),
                        vis.importer_source.clone(),
                        Box::new(ErrorPrivateImport::new(
                            vis.span.clone(),
                            selector.clone(),
                            dep.name.clone(),
                            #[cfg(feature = "debug")]
                            format!("{} {}:{}", file!(), line!(), column!()),
                        )),
                    )));
                }
                None => {
                    errors.push(Box::new(ScopedReport::new(
                        vis.importer_name.clone(),
                        vis.importer_source.clone(),
                        Box::new(ErrorUnresolvedImport::new(
                            vis.span.clone(),
                            selector.clone(),
                            format!(
                                "no public item named `{}` in module `{}`",
                                selector, dep.name
                            ),
                            #[cfg(feature = "debug")]
                            format!("{} {}:{}", file!(), line!(), column!()),
                        )),
                    )));
                }
            }
        }

        let mut adjacency: Vec<Vec<usize>> = vec![Vec::new(); modules.len()];
        let mut edge_span: HashMap<(usize, usize), (Span, usize)> = HashMap::new();
        for (importer, dep_canonical, span) in &raw_edges {
            let Some(&dep_idx) = index_of.get(dep_canonical) else {
                continue;
            };
            if dep_idx == usize::MAX || dep_idx == *importer {
                continue;
            }
            adjacency[*importer].push(dep_idx);
            edge_span
                .entry((*importer, dep_idx))
                .or_insert((span.clone(), *importer));
        }

        // Cycles are forbidden.
        if let Some(cycle) = detect_cycle(&adjacency) {
            let names: Vec<String> = cycle.iter().map(|&i| modules[i].name.clone()).collect();
            // Point the diagnostic at the edge that closes the cycle.
            let (from, to) = (cycle[cycle.len() - 2], cycle[cycle.len() - 1]);
            let (span, importer) = edge_span
                .get(&(from, to))
                .cloned()
                .unwrap_or((Span::new(entry.to_str().unwrap_or_default()), cycle[0]));
            errors.push(Box::new(ScopedReport::new(
                modules[importer].display(),
                modules[importer].source.clone(),
                Box::new(ErrorImportCycle::new(
                    span,
                    names,
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )),
            )));
        }

        if !errors.is_empty() {
            return Err(Box::new(Errors { errors }));
        }

        Ok(LoadedProgram { modules })
    }

    fn parse_module(&self, path: &Path) -> Result<LoadedModule> {
        let raw = std::fs::read_to_string(path).map_err(|err| -> Box<dyn Report> {
            Box::new(ErrorMessage(format!(
                "could not read `{}`: {}",
                path.display(),
                err
            )))
        })?;
        let source = if self.unix_newlines {
            raw.replace("\r\n", "\n")
        } else {
            raw
        };

        let tokens = crate::stage::lexer::Lexer.run((path.to_str().unwrap_or_default(), &source));
        let items = crate::stage::parser::Parser::default()
            .run((path.to_str().unwrap_or_default(), tokens))
            .map_err(|err| -> Box<dyn Report> {
                Box::new(ScopedReport::new(
                    path.display().to_string(),
                    source.clone(),
                    err,
                ))
            })?;

        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("module")
            .to_string();

        Ok(LoadedModule {
            path: path.to_path_buf(),
            name,
            source,
            items,
        })
    }
}

struct PendingVis {
    importer_name: String,
    importer_source: String,
    dep_canonical: PathBuf,
    selector: Option<String>,
    span: Span,
}

/// Resolve a `use` path to a file using "longest file prefix wins". Returns the
/// resolved file and any trailing in-file selectors.
fn resolve_use_path(base_dir: &Path, segments: &[String]) -> Option<(PathBuf, Vec<String>)> {
    for len in (1..=segments.len()).rev() {
        let mut candidate = base_dir.to_path_buf();
        for seg in &segments[0..len] {
            candidate.push(seg);
        }
        candidate.set_extension("cb");
        if candidate.is_file() {
            let selectors = segments[len..].to_vec();
            return Some((candidate, selectors));
        }
    }
    None
}

fn item_visibility(items: &[Item], name: &str) -> Option<Visibility> {
    for item in items {
        match item {
            Item::Function(f) if f.name.lexeme == name => return Some(f.visibility),
            Item::ExternFunction(f) if f.name() == name => return Some(f.visibility),
            Item::Type(ast::TypeDef::Struct(s)) if s.name.lexeme == name => {
                return Some(s.visibility);
            }
            _ => {}
        }
    }
    None
}

fn use_span(use_item: &ast::Use) -> Span {
    match (use_item.path.first(), use_item.path.last()) {
        (Some(first), Some(last)) => Span {
            start: first.span.start,
            end: last.span.end,
            filename: first.span.filename.clone(),
        },
        _ => use_item.use_token.span.clone(),
    }
}

fn canonicalize_or(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A throwaway directory for one test, named uniquely so tests can run in
    /// parallel without colliding.
    struct TmpDir(PathBuf);

    impl TmpDir {
        fn new(tag: &str) -> Self {
            let dir =
                std::env::temp_dir().join(format!("cflat_loader_{}_{}", std::process::id(), tag));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            Self(dir)
        }

        fn write(&self, name: &str, contents: &str) -> PathBuf {
            let path = self.0.join(name);
            std::fs::write(&path, contents).unwrap();
            path
        }
    }

    impl Drop for TmpDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    const MATH: &str =
        "pub fn add(a: s32, b: s32) s32 { return a + b; }\nfn secret(x: s32) s32 { return x; }\n";

    #[test]
    fn resolves_item_import() {
        let dir = TmpDir::new("item");
        dir.write("math.cb", MATH);
        let entry = dir.write(
            "main.cb",
            "use math::add;\npub fn main() s32 { return add(1, 2); }\n",
        );
        let program = ModuleLoader::new(false).load(&entry).expect("should load");
        assert_eq!(program.modules.len(), 2);
        assert_eq!(program.modules[0].name, "main");
    }

    #[test]
    fn resolves_whole_module_import() {
        let dir = TmpDir::new("whole");
        dir.write("math.cb", MATH);
        let entry = dir.write(
            "main.cb",
            "use math;\npub fn main() s32 { return math::add(1, 2); }\n",
        );
        assert!(ModuleLoader::new(false).load(&entry).is_ok());
    }

    #[test]
    fn longest_prefix_wins() {
        // `use a::b` should resolve to a/b.cb (length 2), not a.cb.
        let dir = TmpDir::new("prefix");
        std::fs::create_dir_all(dir.0.join("a")).unwrap();
        dir.write("a.cb", "pub fn wrong() s32 { return 0; }\n");
        dir.write("a/b.cb", "pub fn right() s32 { return 1; }\n");
        let entry = dir.write(
            "main.cb",
            "use a::b::right;\npub fn main() s32 { return right(); }\n",
        );
        let program = ModuleLoader::new(false).load(&entry).expect("should load");
        // main + a/b.cb (a.cb is never imported)
        assert_eq!(program.modules.len(), 2);
        assert!(program.modules.iter().any(|m| m.name == "b"));
    }

    #[test]
    fn unresolved_import_errors() {
        let dir = TmpDir::new("missing");
        let entry = dir.write("main.cb", "use nope::x;\npub fn main() s32 { return 0; }\n");
        assert!(ModuleLoader::new(false).load(&entry).is_err());
    }

    #[test]
    fn private_import_errors() {
        let dir = TmpDir::new("private");
        dir.write("math.cb", MATH);
        let entry = dir.write(
            "main.cb",
            "use math::secret;\npub fn main() s32 { return secret(1); }\n",
        );
        assert!(ModuleLoader::new(false).load(&entry).is_err());
    }

    #[test]
    fn cycle_errors() {
        let dir = TmpDir::new("cycle");
        let entry = dir.write(
            "a.cb",
            "use b;\npub fn fa() s32 { return 1; }\npub fn main() s32 { return fb(); }\n",
        );
        dir.write("b.cb", "use a;\npub fn fb() s32 { return fa(); }\n");
        assert!(ModuleLoader::new(false).load(&entry).is_err());
    }

    #[test]
    fn diamond_is_deduped() {
        let dir = TmpDir::new("diamond");
        dir.write("c.cb", "pub fn cc() s32 { return 0; }\n");
        dir.write("a.cb", "use c;\npub fn aa() s32 { return c::cc(); }\n");
        dir.write("b.cb", "use c;\npub fn bb() s32 { return c::cc(); }\n");
        let entry = dir.write(
            "main.cb",
            "use a;\nuse b;\npub fn main() s32 { return a::aa() + b::bb(); }\n",
        );
        let program = ModuleLoader::new(false).load(&entry).expect("should load");
        // main, a, b, c — c imported by both a and b but parsed once.
        assert_eq!(program.modules.len(), 4);
    }
}

/// Iterative DFS back-edge detection. Returns the cycle as a list of module
/// indices `[a, b, ..., a]` if one exists.
fn detect_cycle(adjacency: &[Vec<usize>]) -> Option<Vec<usize>> {
    let n = adjacency.len();
    let mut color = vec![0u8; n];
    let mut parent = vec![usize::MAX; n];

    for start in 0..n {
        if color[start] != 0 {
            continue;
        }
        color[start] = 1;
        let mut stack: Vec<(usize, usize)> = vec![(start, 0)];
        while let Some(&(node, i)) = stack.last() {
            if i < adjacency[node].len() {
                stack.last_mut().unwrap().1 += 1;
                let next = adjacency[node][i];
                match color[next] {
                    0 => {
                        color[next] = 1;
                        parent[next] = node;
                        stack.push((next, 0));
                    }
                    1 => {
                        let mut chain = vec![node];
                        let mut cur = node;
                        while cur != next && cur != usize::MAX {
                            cur = parent[cur];
                            chain.push(cur);
                        }
                        chain.reverse();
                        chain.push(next);
                        return Some(chain);
                    }
                    _ => {}
                }
            } else {
                color[node] = 2;
                stack.pop();
            }
        }
    }
    None
}
