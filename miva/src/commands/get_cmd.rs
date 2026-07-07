use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{bail, Context, Result};

use super::{color, env};

#[derive(clap::Args)]
pub struct Args {
    /// Repository URL or local path to a git repository
    pub repo: String,
    /// Git tag, branch, or version to install
    pub version: String,
}

fn extract_library_name(repo: &str) -> String {
    // Strip trailing .git
    let trimmed = repo.strip_suffix(".git").unwrap_or(repo);
    // Strip trailing slash
    let trimmed = trimmed.strip_suffix('/').unwrap_or(trimmed);
    // Take the last path segment
    trimmed
        .split('/')
        .last()
        .unwrap_or(trimmed)
        .to_string()
}

fn target_dir(std_include: &Path, name: &str, version: &str) -> PathBuf {
    std_include.join(format!("{}-{}", name, version))
}

pub fn exec(args: Args, verbose: bool) -> Result<()> {
    let name = extract_library_name(&args.repo);
    let std_include = env::get_std_include_dir();
    let target = target_dir(&std_include, &name, &args.version);

    if target.exists() {
        eprintln!(
            "{}",
            color::warn(&format!(
                "library '{}-{}' already exists at {}",
                name,
                args.version,
                target.display()
            ))
        );
        return Ok(());
    }

    // Ensure parent directory exists
    std::fs::create_dir_all(&std_include)
        .context("failed to create std include directory")?;

    // First try with a "v" prefix (matching git tag convention like v0.1.0),
    // then fall back to the exact version string as-is.
    let git_version_prefixed = format!("v{}", &args.version);
    let versions_to_try = [git_version_prefixed.as_str(), args.version.as_str()];

    let mut success = false;
    for ver in &versions_to_try {
        if verbose {
            eprintln!(
                "{} cloning {} @ {} -> {}",
                color::info("ℹ"),
                args.repo,
                ver,
                target.display()
            );
        }

        let status = Command::new("git")
            .arg("clone")
            .arg("--depth")
            .arg("1")
            .arg("--branch")
            .arg(ver)
            .arg(&args.repo)
            .arg(&target)
            .status()
            .context("failed to execute git — is git installed?")?;

        if status.success() {
            success = true;
            break;
        }

        // Clean up partial clone before retrying
        let _ = std::fs::remove_dir_all(&target);
    }

    if !success {
        bail!(
            "git clone failed for {} @ {}",
            args.repo,
            args.version
        );
    }

    // Remove .git directory to keep std_include clean
    let git_dir = target.join(".git");
    if git_dir.exists() {
        if let Err(e) = std::fs::remove_dir_all(&git_dir) {
            eprintln!(
                "{} failed to remove .git from {}: {}",
                color::warn("⚠"),
                target.display(),
                e
            );
        }
    }

    eprintln!(
        "{} installed {}-{}",
        color::success("✓"),
        name,
        args.version
    );

    Ok(())
}
