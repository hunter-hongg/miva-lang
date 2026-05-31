#[derive(clap::Args)]
pub struct Args {
    pub files: Vec<String>,
}

use super::color;

pub fn exec(args: Args, _verbose: bool) -> anyhow::Result<()> {
    let _files = args.files;
    eprintln!("{}", color::errize("miva test: not yet implemented (stub)"));
    Ok(())
}
