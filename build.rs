use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn collect(dir: &Path, root: &Path, out: &mut Vec<(String, PathBuf)>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, root, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("cb") {
            let rel = path
                .strip_prefix(root)
                .expect("std file is under std root")
                .components()
                .map(|c| c.as_os_str().to_string_lossy().into_owned())
                .collect::<Vec<_>>()
                .join("/");
            out.push((rel, path));
        }
    }
}

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR"));
    let std_root = manifest_dir.join("std");
    println!("cargo:rerun-if-changed={}", std_root.display());

    let mut files = Vec::new();
    collect(&std_root, &std_root, &mut files);
    files.sort();

    let mut generated = String::from("pub static STD_FILES: &[(&str, &str)] = &[\n");
    for (rel, path) in &files {
        println!("cargo:rerun-if-changed={}", path.display());
        generated.push_str(&format!(
            "    ({:?}, include_str!({:?})),\n",
            rel,
            path.display().to_string()
        ));
    }
    generated.push_str("];\n");

    let out = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR")).join("embedded_std.rs");
    fs::write(out, generated).expect("write embedded_std.rs");
}
