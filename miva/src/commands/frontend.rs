use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn exe_dir() -> Option<PathBuf> {
    env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))
}

pub fn find_frontend() -> Option<(String, Option<String>)> {
    let base = exe_dir();

    if let Some(ref dir) = base {
        // Platform-specific suffixes used when miva is installed via miver
        let platform_names = [
            "miva-frontend",
            "miva-frontend-linux",
            "miva-frontend-macos",
            "miva-frontend-windows.exe",
        ];

        let candidates = [
            // First, check the canonical frontend-rs build location (relative to miva binary)
            dir.join("../../../miva-frontend-rs/target/debug/miva-frontend"),
            dir.join("../../../miva-frontend-rs/target/release/miva-frontend"),
        ];
        for c in &candidates {
            if c.exists() {
                return Some((c.to_string_lossy().to_string(), None));
            }
        }

        // Check for frontend bundled alongside the miva binary (with platform suffixes)
        for name in &platform_names {
            let c = dir.join(name);
            if c.exists() {
                return Some((c.to_string_lossy().to_string(), None));
            }
        }
    }

    // Fallback: cwd-relative paths
    let cwd_candidates = [
        "../miva-frontend-rs/target/debug/miva-frontend",
        "../miva-frontend-rs/target/release/miva-frontend",
        "miva-frontend-rs/target/debug/miva-frontend",
        "miva-frontend-rs/target/release/miva-frontend",
        "../../miva-frontend-rs/target/debug/miva-frontend",
        "../../miva-frontend-rs/target/release/miva-frontend",
    ];
    for c in &cwd_candidates {
        if Path::new(c).exists() {
            return Some((c.to_string(), None));
        }
    }

    None
}

/// Locate the `mvm` virtual machine binary.
///
/// All search paths are relative to the miva executable itself (via `exe_dir`),
/// NOT the current working directory. This mirrors how `find_frontend` resolves
/// sibling repositories (miva-vm lives at the same level as miva-frontend-rs,
/// i.e. a sibling of the `miva` crate, so three `..` are needed from the
/// executable dir). Order:
///   1. `../../../miva-vm/target/debug/mvm`
///   2. `../../../miva-vm/target/release/mvm`
///   3. `./mvm` (alongside the miva binary)
pub fn find_mvm() -> Option<PathBuf> {
    let base = exe_dir()?;

    let candidates = [
        base.join("../../../miva-vm/target/debug/mvm"),
        base.join("../../../miva-vm/target/release/mvm"),
        base.join("./mvm"),
    ];

    for c in &candidates {
        if c.exists() {
            return Some(c.clone());
        }
    }

    None
}

pub fn run_frontend(
    frontend: &str,
    _work_dir: &Option<String>,
    input: &str,
) -> anyhow::Result<String> {
    let output = Command::new(frontend).arg(input).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("miva-frontend failed:\n{}", stderr);
    }

    String::from_utf8(output.stdout)
        .map_err(|e| anyhow::anyhow!("Invalid UTF-8 from miva-frontend: {}", e))
}
