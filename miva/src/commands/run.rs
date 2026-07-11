use std::path::Path;
use std::process::Command;

use anyhow::Result;

use super::{build, color, env, frontend};
use crate::codegen::Backend;
use crate::config::Config;

#[derive(clap::Args)]
pub struct Args {
    #[arg(
        long,
        help = "Build with the MVM backend and run the program via the mvm interpreter"
    )]
    pub mvm: bool,

    #[arg(
        short = 'b',
        long,
        help = "Backend to use: cxx (default), llvm, or mvm. Overrides miva.toml project.backend. '-b mvm' is equivalent to --mvm."
    )]
    pub backend: Option<String>,
}

pub fn exec(verbose: bool, release: bool, mvm: bool, backend_opt: Option<String>) -> Result<()> {
    if !Path::new("miva.toml").exists() {
        eprintln!(
            "{}",
            color::error("project not initialized in this directory")
        );
        std::process::exit(1);
    }

    // `-b mvm` is equivalent to `--mvm`.
    let mvm = mvm || backend_opt.as_deref() == Some("mvm");

    // Determine which backend to use: the --mvm flag (or `-b mvm`) forces the
    // MVM backend, otherwise the -b option (if given) or the backend declared
    // in miva.toml (default cxx) selects the native backend.
    let backend = if mvm {
        Backend::Mvm
    } else if let Some(b) = &backend_opt {
        Backend::from_name(b)
            .ok_or_else(|| anyhow::anyhow!("Unknown backend '{}'. Use 'cxx' or 'llvm'.", b))?
    } else {
        let config = Config::load().ok_or_else(|| anyhow::anyhow!("failed to parse miva.toml"))?;
        let backend_name = config
            .project_backend()
            .unwrap_or_else(|| "cxx".to_string());
        Backend::from_name(&backend_name).unwrap_or(Backend::Cxx)
    };

    // Build first. When --mvm is given, build the MVM bytecode artifact
    // (build/{debug,release}/<name>.mvm) via `miva build -b mvm`.
    let cli_backend = if mvm {
        Some("mvm".to_string())
    } else {
        backend_opt.clone()
    };
    build::exec(verbose, release, cli_backend)?;

    let config = Config::load().ok_or_else(|| anyhow::anyhow!("failed to parse miva.toml"))?;
    let project = config
        .project
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("missing [project] section in miva.toml"))?;
    let project_type = project
        .project_type
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("missing project.type in miva.toml"))?;

    if project_type == "lib" {
        eprintln!("{}", color::error("cannot run a library project"));
        std::process::exit(1);
    }

    let name = project
        .name
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("missing project.name in miva.toml"))?;
    let build_dir = env::get_build_dir_rel(release);

    // For MVM backend, run the bytecode with the mvm interpreter
    if backend == Backend::Mvm {
        let mvm_path = build_dir.join(format!("{}.mvm", name));
        if !mvm_path.exists() {
            anyhow::bail!("MVM bytecode not found at {}", mvm_path.display());
        }

        let mvm_bin = frontend::find_mvm().ok_or_else(|| {
            anyhow::anyhow!(
                "mvm interpreter not found. Searched (relative to the miva executable):\n  - ../../../miva-vm/target/debug/mvm\n  - ../../../miva-vm/target/release/mvm\n  - ./mvm"
            )
        })?;

        let status = Command::new(&mvm_bin)
            .arg(&mvm_path)
            .status()
            .map_err(|e| anyhow::anyhow!("failed to run mvm: {}", e))?;

        if !status.success() {
            std::process::exit(status.code().unwrap_or(1));
        }
    } else {
        // For native backends (CXX, LLVM), run the compiled binary
        let exe_path = build_dir.join(name);

        let status = std::process::Command::new(&exe_path)
            .status()
            .map_err(|e| anyhow::anyhow!("failed to execute {}: {}", exe_path.display(), e))?;

        if !status.success() {
            std::process::exit(status.code().unwrap_or(1));
        }
    }

    Ok(())
}
