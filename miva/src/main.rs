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

use anyhow::{Context, Result};
use clap::Parser;
use commands::{build, clean, dep, get_cmd, init_cmd, reinit_cmd, run, sin_build, sin_run, test_cmd};

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
    /// Regenerate miva.toml from a template and remove miva.lock
    Reinit(reinit_cmd::Args),
    /// Compile the Miva project to an executable or a library
    Build(build::Args),
    /// Compile and Run the Miva project
    Run(run::Args),
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

/// Names of the built-in subcommands. Built-ins always take precedence over
/// custom scripts defined in the `[scripts]` section of `miva.toml`.
const BUILTIN_COMMANDS: &[&str] = &[
    "init", "build", "run", "clean", "sin-build", "sin-run", "get", "dep", "test", "reinit",
];

fn is_builtin_command(name: &str) -> bool {
    BUILTIN_COMMANDS.contains(&name)
}

/// Execute a custom script from the `[scripts]` section of `miva.toml`.
fn run_script(name: &str, script: &str) -> Result<()> {
    eprintln!("{}", color::info(&format!("running script `{name}`: {script}")));
    let status = std::process::Command::new(if cfg!(windows) { "cmd" } else { "sh" })
        .arg(if cfg!(windows) { "/C" } else { "-c" })
        .arg(script)
        .status()
        .with_context(|| format!("failed to execute script `{name}`"))?;

    match status.code() {
        Some(0) => Ok(()),
        Some(code) => std::process::exit(code),
        None => std::process::exit(1),
    }
}

fn main() {
    let raw_args: Vec<String> = std::env::args().collect();
    if let Some(cmd) = raw_args.get(1) {
        // Let clap handle flags (e.g. --help, --version) and built-ins directly.
        if !cmd.starts_with('-') {
            if is_builtin_command(cmd) {
                // A script with the same name as a built-in is a collision:
                // warn the user but run the built-in command instead.
                if let Some(cfg) = config::Config::load() {
                    if let Some(scripts) = &cfg.scripts {
                        if scripts.contains_key(cmd) {
                            eprintln!(
                                "{}",
                                color::warn(&format!(
                                    "script `{cmd}` collides with built-in command; \
                                     running built-in command instead"
                                ))
                            );
                        }
                    }
                }
            } else if let Some(cfg) = config::Config::load() {
                // Not a built-in: try to resolve it as a custom script.
                if let Some(scripts) = &cfg.scripts {
                    if let Some(script) = scripts.get(cmd) {
                        if let Err(e) = run_script(cmd, script) {
                            eprintln!("{}", color::error(&e.to_string()));
                        }
                        return;
                    }
                }
            }
        }
    }

    let cli = Cli::parse();

    let res = match cli.command {
        Command::Init(args) => init_cmd::exec(args, cli.verbose),
        Command::Reinit(args) => reinit_cmd::exec(args, cli.verbose),
        Command::Build(args) => build::exec(cli.verbose, cli.release, args.backend),
        Command::Run(args) => run::exec(cli.verbose, cli.release, args.mvm, args.backend),
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
