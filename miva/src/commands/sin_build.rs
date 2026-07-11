#[derive(clap::Args)]
pub struct Args {
    pub input: String,
    #[arg(short)]
    pub output: Option<String>,
    #[arg(short = 'b', long, default_value = "cxx", help = "Backend to use: cxx, llvm, or mvm")]
    pub backend: String,
}

pub fn exec(_args: Args, _verbose: bool) -> anyhow::Result<()> {
    anyhow::bail!("sin-build is deprecated. Please create a complete project and use the `build` command instead.");
}
