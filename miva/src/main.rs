mod ast;
mod codegen;
mod commands;
mod config;
mod error;
mod json_ast;
mod macro_expand;
mod magical;
mod semantic;
mod symbol_table;
mod typecheck;
mod warning;

use clap::Parser;
use commands::{build, clean, dep, get_cmd, init_cmd, run, sin_build, sin_run, test_cmd};

use crate::commands::color;

#[derive(Parser)]
#[command(
    name = "miva",
    version,
    about = "The Miva programming language compiler"
)]
struct Cli {
    #[arg(short, long, global = true, help = "Enable verbose output")]
    verbose: bool,

    #[arg(short, long, global = true, help = "Release mode")]
    release: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Initialize a new Miva project
    Init(init_cmd::Args),
    /// Compile the Miva project to an executable or a library
    Build,
    /// Compile and Run the Miva project
    Run,
    /// Clean the build artifacts in a Miva project
    Clean,
    /// Compile an Miva source file to an executable
    #[command(name = "sin-build")]
    SinBuild(sin_build::Args),
    /// Compile and run an Miva program
    #[command(name = "sin-run")]
    SinRun(sin_run::Args),
    /// Install a library from a GitHub repository
    Get(get_cmd::Args),
    /// Show complete dependency graph starting from main.miva
    Dep,
    /// Run all tests in the Miva project or specific files
    Test(test_cmd::Args),
}

fn main() {
    let cli = Cli::parse();

    let res = match cli.command {
        Command::Init(args) => init_cmd::exec(args, cli.verbose),
        Command::Build => build::exec(cli.verbose, cli.release),
        Command::Run => run::exec(cli.verbose, cli.release),
        Command::Clean => clean::exec(cli.verbose),
        Command::SinBuild(args) => sin_build::exec(args, cli.verbose),
        Command::SinRun(args) => sin_run::exec(args, cli.verbose),
        Command::Get(args) => get_cmd::exec(args, cli.verbose),
        Command::Dep => dep::exec(cli.verbose),
        Command::Test(args) => test_cmd::exec(args, cli.verbose),
    };

    if let Some(e) = res.err() {
        let err = e.to_string();
        eprintln!("{}", color::error(&err));
    }
}
