use std::path::Path;

use anyhow::Result;

use super::color;

pub fn exec(_verbose: bool) -> Result<()> {
    if !Path::new("miva.toml").exists() {
        eprintln!("{}", color::errize("project not initialized in this directory"));
        std::process::exit(1);
    }
    let build_dir = std::env::var("MIVA_BUILD_BASIC").unwrap_or_else(|_| "build".to_string());
    if Path::new(&build_dir).exists() {
        std::fs::remove_dir_all(&build_dir)?;
    }
    eprintln!("{}", color::infoize("cleaned build artifacts"));
    Ok(())
}
