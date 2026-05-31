#[derive(clap::Args)]
pub struct Args {
    pub input: String,
}

use super::color;

pub fn exec(args: Args, _verbose: bool) -> anyhow::Result<()> {
    let _input = args.input;
    eprintln!("{}", color::errize("miva sin-run: not yet implemented (stub)"));
    Ok(())
}
