use std::path::PathBuf;

use super::MemorySource;

include!(concat!(env!("OUT_DIR"), "/embedded_std.rs"));

pub const STD_ROOT: &str = "std";

pub fn memory_source(entry: &str, source: &str) -> MemorySource {
    let mut memory = MemorySource::new(Some(PathBuf::from(STD_ROOT)));
    for (rel, contents) in STD_FILES {
        memory.insert(format!("{STD_ROOT}/{rel}"), *contents);
    }
    memory.insert(entry, source);
    memory
}

pub fn lookup(filename: &str) -> Option<&'static str> {
    let rel = filename.strip_prefix(STD_ROOT)?.strip_prefix('/')?;
    STD_FILES
        .iter()
        .find(|(path, _)| *path == rel)
        .map(|(_, contents)| *contents)
}
