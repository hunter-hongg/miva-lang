use std::fs;
use std::path::Path;

use anyhow::{bail, Context, Result};

use super::color;

#[derive(clap::Args)]
pub struct Args {
    pub name: String,
    #[arg(short = 't')]
    pub type_: String,
}

fn toml_content(name: &str, type_: &str) -> String {
    format!(
        "[project]
name = \"{name}\"
version = \"0.1.0\"
type = \"{type_}\"

[env]

[scripts]
dev = \"miva run -b mvm\"
release = \"miva build -b llvm --release\"

[dependencies]
std = \"0.1.3\"
"
    )
}

pub fn exec(args: Args, _verbose: bool) -> Result<()> {
    if !Path::new("miva.toml").exists() {
        bail!("project not initialized in this directory");
    }

    match args.type_.as_str() {
        "bin" | "lib" => {}
        _ => bail!("unknown project type {}", args.type_),
    }

    fs::write("miva.toml", toml_content(&args.name, &args.type_))
        .context("failed to write miva.toml")?;

    if Path::new("miva.lock").exists() {
        fs::remove_file("miva.lock").context("failed to remove miva.lock")?;
    }

    eprintln!(
        "{}",
        color::info(&format!(
            "reinitialized {} project '{}'",
            args.type_, args.name
        ))
    );
    Ok(())
}
