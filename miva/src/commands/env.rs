use std::path::PathBuf;
use sha2::{Sha256, Digest};

pub fn compute_sha256(path: &str) -> String {
    let content = std::fs::read(path).unwrap_or_default();
    let mut hasher = Sha256::new();
    hasher.update(&content);
    let result = hasher.finalize();
    hex::encode(result)
}

pub fn hash_file_path(cache_dir: &PathBuf, cache_key: &str) -> PathBuf {
    cache_dir.join(format!("{}.sha256", cache_key))
}

pub fn get_std_include_dir() -> PathBuf {
    if let Ok(dir) = std::env::var("MIVA_STD") {
        PathBuf::from(dir)
    } else if let Ok(home) = std::env::var("HOME") {
        PathBuf::from(format!("{}/.miver/lib/", home))
    } else {
        PathBuf::from("/.miver/lib/")
    }
}

pub fn get_build_dir() -> PathBuf {
    std::env::var("MIVA_BUILD")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("build/debug"))
}

pub fn get_cache_dir() -> PathBuf {
    std::env::var("MIVA_BUILD_CACHE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("build/debug/cache"))
}

pub fn get_build_dir_rel(release: bool) -> PathBuf {
    if release {
        std::env::var("MIVA_RELEASE_BUILD")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("build/release"))
    } else {
        get_build_dir()
    }
}

pub fn get_cache_dir_rel(release: bool) -> PathBuf {
    if release {
        std::env::var("MIVA_RELEASE_CACHE")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("build/release/cache"))
    } else {
        get_cache_dir()
    }
}

pub fn get_include_flags() -> String {
    std::env::var("MIVA_INC_FLAGS").unwrap_or_default()
}

pub fn get_link_flags() -> String {
    std::env::var("MIVA_LINK_FLAGS").unwrap_or_default()
}

pub fn get_keep_cpp() -> bool {
    std::env::var("MIVA_KEEP_CPP").is_ok()
}

