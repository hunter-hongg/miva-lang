#[derive(clap::Args)]
pub struct Args {
    pub input: String,
}

use super::color;

pub fn exec(args: Args, _verbose: bool) -> anyhow::Result<()> {
    let _input = args.input;
    eprintln!("{}", color::error("miva sin-run: not yet implemented (stub)"));
    Ok(())
}
