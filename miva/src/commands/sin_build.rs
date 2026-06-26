use std::path::Path;
use std::process::Command;

use super::{color, env, frontend};
use crate::codegen;
use crate::json_ast;
use crate::macro_expand;
use crate::magical;
use crate::semantic;
use crate::typecheck;
use crate::warning;

#[derive(clap::Args)]
pub struct Args {
    pub input: String,
    #[arg(short)]
    pub output: Option<String>,
}

pub fn exec(args: Args, verbose: bool) -> anyhow::Result<()> {
    let input = &args.input;

    if !Path::new(input).exists() {
        anyhow::bail!("input file not found: {}", input);
    }

    let (frontend, work_dir) = frontend::find_frontend().ok_or_else(|| {
        anyhow::anyhow!("miva-frontend not found. Build it with: cd ../miva-frontend && dune build")
    })?;

    let cache_dir = env::get_cache_dir_rel(false);
    let build_dir = env::get_build_dir_rel(false);
    std::fs::create_dir_all(&cache_dir)?;
    std::fs::create_dir_all(&build_dir)?;

    let cache_key = input.replace('/', "_");
    let cpp_path = cache_dir.join(format!("{}.cpp", cache_key));
    let obj_path = cache_dir.join(format!("{}.o", cache_key));

    let needs_recompile = if cpp_path.exists() {
        let hash_path = env::hash_file_path(&cache_dir, &cache_key);
        let current_hash = env::compute_sha256(input);
        if let Ok(stored_hash) = std::fs::read_to_string(&hash_path) {
            stored_hash.trim() != current_hash
        } else {
            true
        }
    } else {
        true
    };

    if needs_recompile {
        if verbose {
            eprintln!("{}", color::step("compile", input));
        }

        let json_str = frontend::run_frontend(&frontend, &work_dir, input)?;
        let ast = json_ast::from_str(&json_str)
            .map_err(|e| anyhow::anyhow!("Failed to parse JSON AST: {}", e))?;

        // Collect macros from the single file before expansion
        let macro_table = macro_expand::collect_macros(&ast.defs);
        let defs = macro_expand::expand_macros(&ast.defs, &macro_table)?;

        let sem_errors = semantic::check_program(&defs);
        if !sem_errors.is_empty() {
            for err in &sem_errors {
                eprintln!("semantic error [{}]: {}", err.code, err.message);
            }
            anyhow::bail!("semantic errors found");
        }

        let type_errors = typecheck::check_program(&defs);
        if !type_errors.is_empty() {
            for err in &type_errors {
                eprintln!(
                    "{}",
                    color::colorize(
                        color::RED,
                        format!("  Error [{}]: {}", err.code, err.message).as_str()
                    )
                );
            }
            anyhow::bail!("type errors found");
        }

        let magical_flags = magical::get_magical_flags(&defs);
        let warnings = warning::get_warnings(&defs);
        let (warnings, err_warnings) = magical::filter_warnings(warnings, &magical_flags);
        for w in &err_warnings {
            eprintln!(
                "{}",
                color::colorize(
                    color::RED,
                    &format!("  warning-as-error [{}]: {}", w.code, w.message)
                )
            );
        }
        if !err_warnings.is_empty() {
            anyhow::bail!("some warnings treated as errors");
        }
        for w in &warnings {
            eprintln!("{}", color::warn(&format!("[{}]: {}", w.code, w.message)));
        }

        let [program, header, test] = codegen::build_ir(&defs);

        std::fs::write(&cpp_path, &program)?;

        if !header.is_empty() {
            let header_path = cache_dir.join(format!("{}.h", cache_key));
            std::fs::write(header_path, &header)?;
        }

        if !test.is_empty() {
            let test_path = cache_dir.join(format!("{}.test.cpp", cache_key));
            let test = if !header.is_empty() {
                let basename = std::path::Path::new(&cache_key)
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or(&cache_key);
                format!("#include \"{}.h\"\n{}", basename, test)
            } else {
                test
            };
            std::fs::write(test_path, &test)?;
        }

        let current_hash = env::compute_sha256(input);
        let hash_path = env::hash_file_path(&cache_dir, &cache_key);
        let _ = std::fs::write(&hash_path, current_hash);
    } else if verbose {
        eprintln!("  {} (cached)", input);
    }

    let needs_obj = if obj_path.exists() {
        let cpp_mtime = std::fs::metadata(&cpp_path)
            .ok()
            .and_then(|m| m.modified().ok());
        let obj_mtime = std::fs::metadata(&obj_path)
            .ok()
            .and_then(|m| m.modified().ok());
        match (cpp_mtime, obj_mtime) {
            (Some(cpp_t), Some(obj_t)) => cpp_t > obj_t,
            _ => true,
        }
    } else {
        true
    };

    if needs_obj {
        let std_include = env::get_std_include_dir();
        let std_inc_flag = format!("-I{}", std_include.to_string_lossy());
        let cache_inc_flag = format!("-I{}", cache_dir.to_string_lossy());
        let opt_flag = "-g";

        let gpp_output = Command::new("g++")
            .args([
                opt_flag,
                "-std=c++20",
                "-c",
                cpp_path.to_str().unwrap(),
                "-o",
                obj_path.to_str().unwrap(),
                std_inc_flag.as_str(),
                cache_inc_flag.as_str(),
            ])
            .output()
            .map_err(|e| anyhow::anyhow!("Failed to run g++: {}", e))?;

        if !gpp_output.status.success() {
            let stderr = String::from_utf8_lossy(&gpp_output.stderr);
            eprintln!(
                "{}",
                color::error(&format!("g++ compilation failed:\n{}", stderr))
            );
            std::process::exit(1);
        }
    }

    let out_path = args.output.unwrap_or_else(|| {
        let stem = Path::new(input)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("a");
        format!("{}.exe", stem)
    });

    let std_include = env::get_std_include_dir();
    let std_inc_flag = format!("-I{}", std_include.to_string_lossy());
    let cache_inc_flag = format!("-I{}", cache_dir.to_string_lossy());

    let link_output = Command::new("g++")
        .args([
            "-O2",
            "-std=c++20",
            "-o",
            &out_path,
            obj_path.to_str().unwrap(),
            std_inc_flag.as_str(),
            cache_inc_flag.as_str(),
        ])
        .output()
        .map_err(|e| anyhow::anyhow!("Failed to run g++ for linking: {}", e))?;

    if !link_output.status.success() {
        let stderr = String::from_utf8_lossy(&link_output.stderr);
        eprintln!("{}", color::error(&format!("linking failed:\n{}", stderr)));
        std::process::exit(1);
    }

    println!(
        "{}",
        color::success(&format!("{} compiled -> {}", input, out_path))
    );
    Ok(())
}
