#[derive(clap::Args)]
pub struct Args {
    pub input: String,
    #[arg(short = 'b', long, default_value = "cxx", help = "Backend to use: cxx, llvm, or mvm")]
    pub backend: String,
}

pub fn exec(_args: Args, _verbose: bool) -> anyhow::Result<()> {
    anyhow::bail!("sin-run is deprecated. Please create a complete project and use the `run` command instead.");
}
