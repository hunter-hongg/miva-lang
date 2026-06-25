use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::Result;

use super::{build, color, env};

#[derive(clap::Args)]
pub struct Args {
    pub files: Vec<String>,
}

fn find_test_files(cache_dir: &Path, files: &[String]) -> Vec<PathBuf> {
    if !files.is_empty() {
        return files
            .iter()
            .map(|f| {
                let p = PathBuf::from(f);
                if p.is_absolute() {
                    p
                } else {
                    cache_dir.join(p)
                }
            })
            .collect();
    }
    let mut results = Vec::new();
    let mut stack = vec![cache_dir.to_path_buf()];
    while let Some(dir) = stack.pop() {
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path.extension().map_or(false, |e| e == "cpp")
                    && path.to_string_lossy().contains(".test.")
                {
                    results.push(path);
                }
            }
        }
    }
    results
}

fn find_obj_files(cache_dir: &Path) -> Vec<PathBuf> {
    let mut objs = Vec::new();
    let mut stack = vec![cache_dir.to_path_buf()];
    while let Some(dir) = stack.pop() {
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path.extension().map_or(false, |e| e == "o") {
                    objs.push(path);
                }
            }
        }
    }
    objs
}

fn compile_test(
    test_file: &Path,
    obj_files: &[PathBuf],
    cache_dir: &Path,
    std_include: &Path,
    inc_flags: &str,
    link_flags: &str,
    output: &Path,
) -> Result<bool> {
    let mut cmd = Command::new("g++");
    cmd.arg("-std=c++20")
        .arg("-g")
        .arg(test_file)
        .arg("-o")
        .arg(output);

    cmd.arg(format!("-I{}", cache_dir.display()));
    cmd.arg(format!("-I{}", std_include.display()));

    for flag in inc_flags.split_whitespace() {
        if !flag.is_empty() {
            cmd.arg(flag);
        }
    }

    for obj in obj_files {
        cmd.arg(obj);
    }

    for flag in link_flags.split_whitespace() {
        if !flag.is_empty() {
            cmd.arg(flag);
        }
    }
    cmd.arg("-Wl,--allow-multiple-definition");

    let out = cmd.output()?;
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        eprintln!(
            "{}",
            color::errize(&format!(
                "test compilation failed for {}:\n{}",
                test_file.display(),
                stderr
            ))
        );
        return Ok(false);
    }
    Ok(true)
}

pub fn exec(args: Args, verbose: bool) -> Result<()> {
    build::exec(verbose, false)?;
    eprintln!("{}", "-".repeat(30));

    let cache_dir = env::get_cache_dir_rel(false);
    let std_include = env::get_std_include_dir();
    let inc_flags = env::get_include_flags();
    let link_flags = env::get_link_flags();

    let test_files = find_test_files(&cache_dir, &args.files);
    if test_files.is_empty() {
        eprintln!("{}", color::colorize(color::YELLOW, "no test files found"));
        return Ok(());
    }

    if verbose {
        eprintln!("  found {} test file(s)", test_files.len());
    }

    let obj_files = find_obj_files(&cache_dir);
    if verbose {
        eprintln!("  found {} object file(s) for linking", obj_files.len());
    }

    let mut passed = 0u32;
    let mut failed = 0u32;
    let total = test_files.len() as u32;

    for test_file in &test_files {
        let test_name = test_file
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .trim_end_matches(".test");

        let exe_path = PathBuf::from("/tmp").join(format!("{}.test", test_name));

        eprintln!("{}", color::logize(color::CYAN, "TESTING", test_name));

        let ok = compile_test(
            test_file,
            &obj_files,
            &cache_dir,
            &std_include,
            &inc_flags,
            &link_flags,
            &exe_path,
        )?;

        if !ok {
            failed += 1;
            continue;
        }

        let run_out = Command::new(&exe_path).output()?;

        let stdout = String::from_utf8_lossy(&run_out.stdout);
        if !stdout.is_empty() {
            print!("{}", stdout);
        }

        let stderr_output = String::from_utf8_lossy(&run_out.stderr);
        if !stderr_output.is_empty() {
            eprint!(
                "{}",
                color::errize(&format!("test stderr [{}]: {}", test_name, stderr_output))
            );
        }

        if run_out.status.success() {
            passed += 1;
        } else {
            failed += 1;
        }

        let _ = std::fs::remove_file(&exe_path);
    }

    if total > 0 {
        println!(
            "{}",
            color::logize(
                color::CYAN,
                "SUMMARY",
                &format!("{}/{} passed, {}/{} failed", passed, total, failed, total)
            )
        );
        if failed > 0 {
            println!("{}", color::logize(color::RED, "ERROR", "test failed"))
        } else {
            println!(
                "{}",
                color::logize(color::GREEN, "SUCCEED", "all tests passed")
            )
        }
    }

    if failed > 0 {
        std::process::exit(1);
    }

    Ok(())
}
