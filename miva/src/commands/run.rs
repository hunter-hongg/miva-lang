use std::path::Path;

use anyhow::Result;

use super::{build, color, env};
use crate::config::Config;

pub fn exec(verbose: bool, release: bool) -> Result<()> {
    if !Path::new("miva.toml").exists() {
        eprintln!("{}", color::errize("project not initialized in this directory"));
        std::process::exit(1);
    }

    build::exec(verbose, release)?;

    let config =
        Config::load().ok_or_else(|| anyhow::anyhow!("failed to parse miva.toml"))?;
    let project = config
        .project
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("missing [project] section in miva.toml"))?;
    let project_type = project
        .project_type
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("missing project.type in miva.toml"))?;

    if project_type == "lib" {
        eprintln!("{}", color::errize("cannot run a library project"));
        std::process::exit(1);
    }

    let name = project
        .name
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("missing project.name in miva.toml"))?;
    let build_dir = env::get_build_dir_rel(release);
    let exe_path = build_dir.join(name);

    let status = std::process::Command::new(&exe_path)
        .status()
        .map_err(|e| anyhow::anyhow!("failed to execute {}: {}", exe_path.display(), e))?;

    if !status.success() {
        std::process::exit(status.code().unwrap_or(1));
    }

    Ok(())
}
