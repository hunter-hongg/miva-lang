use anyhow::Result;

use super::{color, env, lock};
use crate::config::Config;

pub fn exec(_verbose: bool) -> Result<()> {
    let config = Config::load().ok_or_else(|| anyhow::anyhow!("no miva.toml found"))?;
    let declared = config.dependencies();

    if declared.is_empty() {
        eprintln!("{}", color::info("no dependencies declared in miva.toml"));
        return Ok(());
    }

    let std_include = env::get_std_include_dir();
    let resolved = lock::resolve_force(&declared, &std_include)?;

    lock::write_lock(&resolved)?;

    for (name, version) in &resolved {
        eprintln!("  {} {} ({})", color::info("✓"), name, version);
    }

    eprintln!("{}", color::success("dependencies resolved — lock file written"));
    Ok(())
}
