use std::collections::HashSet;
use std::fs::exists;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::Result;

use super::{color, dependency_graph::DependencyGraph, env, frontend};
use crate::codegen;
use crate::commands::clean;
use crate::config::Config;
use crate::json_ast;
use crate::macro_expand;
use crate::magical;
use crate::semantic;
use crate::typecheck;
use crate::warning;

fn find_project_root() -> Option<PathBuf> {
    let mut current = std::env::current_dir().ok()?;
    loop {
        if current.join("miva.toml").exists() {
            return Some(current);
        }
        if !current.pop() {
            return None;
        }
    }
}

fn determine_entry(project_type: &str) -> &str {
    if project_type == "lib" {
        "src/lib.miva"
    } else {
        "src/main.miva"
    }
}

fn dep_cache_path(cache_dir: &Path, file: &str, std_path: &str) -> PathBuf {
    let cache_key = make_cache_key(file, std_path);
    cache_dir.join(format!("{}.d", cache_key))
}

fn read_dep_cache(path: &Path, source: &str) -> Option<Vec<String>> {
    let dep_meta = std::fs::metadata(path).ok()?;
    if let Ok(src_meta) = std::fs::metadata(source) {
        if let (Ok(src_mtime), Ok(dep_mtime)) = (src_meta.modified(), dep_meta.modified()) {
            if src_mtime > dep_mtime {
                return None;
            }
        }
    }
    let content = std::fs::read_to_string(path).ok()?;
    Some(content.lines().map(|l| l.to_string()).collect())
}

fn _write_dep_cache(path: &Path, deps: &[String]) {
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let content = deps.join("\n");
    let _ = std::fs::write(path, content);
}

fn collect_imports_with_graph(
    frontend: &str,
    work_dir: &Option<String>,
    file: &str,
    visited: &mut HashSet<String>,
    cache_dir: &Path,
    std_path: &str,
    graph: &mut DependencyGraph,
    name: &str,
) -> Result<()> {
    let mut file_parts: Vec<_> = file.split('/').collect();
    let _file = file;
    let file: String;
    if exists(_file)? {
        file = _file.to_string();
    } else if file_parts.is_empty() {
        color::error("cannot parse import file");
        anyhow::bail!("parsing failed")
    } else {
        let first = *file_parts.get(0).unwrap_or(&"");
        file_parts.remove(0);
        match first {
            _ if first == name => {
                file = "src/".to_string() + file_parts.join("/").as_str() + ".miva";
            }
            _ => {
                let inc = format!("{}", env::get_std_include_dir().display());
                file = inc + "/" + first + "/src/" + file_parts.join("/").as_str() + ".miva";
            }
        }
    }

    if !visited.insert(file.clone()) {
        return Ok(());
    }

    let file = file.as_str();
    let dep_path = dep_cache_path(cache_dir, file, std_path);
    let deps = read_dep_cache(&dep_path, file);

    let import_paths: Vec<String> = if let Some(deps) = deps {
        deps
    } else {
        let json_str = frontend::run_frontend(frontend, work_dir, file)?;
        let ast = json_ast::from_str(&json_str)
            .map_err(|e| anyhow::anyhow!("Failed to parse JSON AST from {}: {}", file, e))?;

        let paths: Vec<String> = ast
            .defs
            .iter()
            .filter_map(|def| match def {
                crate::ast::Def::SImport { path, .. }
                | crate::ast::Def::SImportAs { path, .. }
                | crate::ast::Def::SImportHere { path, .. } => Some(path.clone()),
                _ => None,
            })
            .collect();

        paths
    };

    for path in &import_paths {
        graph.add_dependency(file, path);
        collect_imports_with_graph(
            frontend, work_dir, path, visited, cache_dir, std_path, graph, name,
        )?;
    }

    Ok(())
}

/// Collect all macro definitions from all source files.
///
/// Runs the frontend on each file, parses the JSON AST, and extracts
/// `DMacro` definitions into a shared `MacroTable`. This is called once
/// before per-file compilation so that every file's macro expansion has
/// access to macros defined anywhere in the project.
fn collect_all_macros(
    frontend: &str,
    work_dir: &Option<String>,
    files: &[String],
) -> macro_expand::MacroTable {
    let mut table = macro_expand::MacroTable::new();
    for file in files {
        match frontend::run_frontend(frontend, work_dir, file) {
            Ok(json_str) => {
                if let Ok(ast) = crate::json_ast::from_str(&json_str) {
                    let file_macros = macro_expand::collect_macros(&ast.defs);
                    for (name, def) in file_macros {
                        table.entry(name).or_insert(def);
                    }
                }
            }
            Err(e) => {
                eprintln!(
                    "Warning: failed to parse {} for macro collection: {}",
                    file, e
                );
            }
        }
    }
    table
}

fn make_cache_key(file: &str, std_path: &str) -> String {
    if let Some(rest) = file.strip_prefix(std_path) {
        rest.trim_start_matches('/').to_string()
    } else if let Some(rest) = file.strip_prefix('/') {
        rest.to_string()
    } else {
        file.to_string()
    }
}

fn needs_rebuild_by_hash(file: &str, cache_dir: &Path, cache_key: &str) -> bool {
    let hash_path = env::hash_file_path(&cache_dir.to_path_buf(), cache_key);
    let current_hash = env::compute_sha256(file);

    if let Ok(stored_hash) = std::fs::read_to_string(&hash_path) {
        let stored_hash = stored_hash.trim();
        stored_hash != current_hash
    } else {
        true
    }
}

fn update_hash_cache(file: &str, cache_dir: &Path, cache_key: &str) {
    let hash_path = env::hash_file_path(&cache_dir.to_path_buf(), cache_key);
    let current_hash = env::compute_sha256(file);
    if let Some(parent) = hash_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::write(&hash_path, current_hash);
}

fn compile_file_to_cpp(
    frontend: &str,
    work_dir: &Option<String>,
    file: &str,
    cache_dir: &Path,
    std_path: &str,
    _verbose: bool,
    macro_table: &macro_expand::MacroTable,
) -> Result<(PathBuf, bool)> {
    let cache_key = make_cache_key(file, std_path);
    let cpp_path = cache_dir.join(format!("{}.cpp", cache_key));

    if cpp_path.exists() && !needs_rebuild_by_hash(file, cache_dir, &cache_key) {
        let has_main = std::fs::read_to_string(&cpp_path)
            .map(|s| s.contains("mvp_own_main"))
            .unwrap_or(false);
        return Ok((cpp_path, has_main));
    }

    let json_str = frontend::run_frontend(frontend, work_dir, file)?;
    let ast = json_ast::from_str(&json_str)
        .map_err(|e| anyhow::anyhow!("Failed to parse JSON AST from {}: {}", file, e))?;

    let defs = macro_expand::expand_macros(&ast.defs, macro_table)?;

    let sem_errors = semantic::check_program(&defs);
    if !sem_errors.is_empty() {
        for err in &sem_errors {
            eprintln!(
                "{}",
                color::colorize(
                    color::RED,
                    format!("Error [{}]: {}", err.code, err.message).as_str()
                )
            );
        }
        eprintln!("{}", "-".repeat(30));
        anyhow::bail!("semantic errors found");
    }

    let type_errors = typecheck::check_program(&defs);
    if !type_errors.is_empty() {
        for err in &type_errors {
            eprintln!(
                "{}",
                color::colorize(
                    color::RED,
                    format!("Error [{}]: {}", err.code, err.message).as_str()
                )
            );
        }
        eprintln!("{}", "-".repeat(30));
        anyhow::bail!("type errors found");
    }

    let magical_flags = magical::get_magical_flags(&defs);
    let warnings = warning::get_warnings(&defs);
    let (warnings, err_warnings) = magical::filter_warnings(warnings, &magical_flags);
    for w in &err_warnings {
        eprintln!(
            "{}",
            color::colorize(color::RED, &format!("Error [{}]: {}", w.code, w.message))
        );
    }
    if !err_warnings.is_empty() {
        eprintln!("{}", "-".repeat(30));
        anyhow::bail!("some warnings treated as errors");
    }
    for w in &warnings {
        eprintln!(
            "{}",
            color::colorize(
                color::YELLOW,
                &format!("Warning [{}]: {}", w.code, w.message)
            )
        );
    }

    let [program, header, test] = codegen::build_ir(&defs);

    std::fs::create_dir_all(cpp_path.parent().unwrap())?;
    std::fs::write(&cpp_path, &program)?;

    if !header.is_empty() {
        let header_path = cache_dir.join(format!("{}.h", cache_key));
        std::fs::write(&header_path, &header)?;
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
        std::fs::write(&test_path, &test)?;
    }

    update_hash_cache(file, cache_dir, &cache_key);

    let has_main = program.contains("mvp_own_main");
    Ok((cpp_path, has_main))
}

fn include_flag(dir: &[&Path]) -> Vec<String> {
    let mut flag = Vec::new();
    for i in dir {
        let f = format!("-I{}", i.to_str().unwrap());
        flag.push(f);
    }
    flag
}

fn compile_cpp_to_obj(
    cpp_path: &Path,
    cache_dir: &Path,
    std_include: &Path,
    build_dir: &Path,
    project_type: &str,
    release: bool,
    verbose: bool,
) -> Result<PathBuf> {
    let obj_path = cpp_path.with_extension("o");

    let opt_flag = if release { "-O2" } else { "-g" };
    let pic_flag = if project_type == "lib" { "-fPIC" } else { "" };
    let inc_flags = env::get_include_flags();
    let include = include_flag(&[cache_dir, std_include, build_dir]);

    let mut args = vec![
        opt_flag,
        "-std=c++20",
        "-c",
        cpp_path.to_str().unwrap(),
        "-o",
        obj_path.to_str().unwrap(),
    ];

    if !pic_flag.is_empty() {
        args.push(pic_flag);
    }
    for flag in inc_flags.split_whitespace() {
        if !flag.is_empty() {
            args.push(flag);
        }
    }
    for flag in &include {
        args.push(flag);
    }

    let mut cmd = Command::new("g++");
    cmd.args(&args);
    if !verbose {
        cmd.stderr(std::process::Stdio::null());
    }
    let compile_output = cmd
        .output()
        .map_err(|e| anyhow::anyhow!("Failed to run g++: {}", e))?;

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr);
        eprintln!("{}", color::error("g++ compilation failed"));
        if verbose {
            eprintln!("{}", stderr);
        } else {
            // Always show at least some error output
            for line in stderr.lines().take(5) {
                eprintln!("{}", line);
            }
        }
        clean::exec(false)?;
        std::process::exit(1);
    }

    Ok(obj_path)
}

fn link_objects(
    obj_files: &[PathBuf],
    output_file: &str,
    build_dir: &Path,
    project_type: &str,
    _release: bool,
) -> Result<String> {
    let exe_name = if project_type == "lib" {
        format!("lib{}", output_file)
    } else {
        output_file.to_string()
    };

    let exe_path = build_dir.join(&exe_name);
    let final_path = if project_type == "lib" {
        exe_path.with_extension("so")
    } else {
        exe_path.clone()
    };

    let mut args = vec!["-O2", "-std=c++20", "-o", final_path.to_str().unwrap()];

    for obj in obj_files {
        args.push(obj.to_str().unwrap());
    }

    if project_type == "lib" {
        args.push("-fPIC");
        args.push("-shared");
    }

    let link_flags = env::get_link_flags();
    for flag in link_flags.split_whitespace() {
        if !flag.is_empty() {
            args.push(flag);
        }
    }

    let output = Command::new("g++")
        .args(&args)
        .output()
        .map_err(|e| anyhow::anyhow!("Failed to run g++ for linking: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("{}", color::error(&format!("linking failed:\n{}", stderr)));
        std::process::exit(1);
    }

    Ok(final_path.to_string_lossy().to_string())
}

pub fn exec(verbose: bool, release: bool) -> Result<()> {
    let project_root = find_project_root()
        .ok_or_else(|| anyhow::anyhow!("no miva.toml found. Run `miva init <name>` first."))?;

    std::env::set_current_dir(&project_root)?;

    let config = Config::load().ok_or_else(|| anyhow::anyhow!("failed to parse miva.toml"))?;

    let project = config
        .project
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("missing [project] section in miva.toml"))?;

    let name = project
        .name
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("missing project.name in miva.toml"))?;

    let project_type = project
        .project_type
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("missing project.type in miva.toml"))?;

    let entry_file = determine_entry(project_type);

    if !Path::new(entry_file).exists() {
        anyhow::bail!("entry file not found: {}", entry_file);
    }

    let (frontend, frontend_work_dir) =
        frontend::find_frontend().ok_or_else(|| anyhow::anyhow!("miva-frontend not found"))?;

    let std_include_dir = env::get_std_include_dir();
    let cache_dir = env::get_cache_dir_rel(release);
    let build_dir = env::get_build_dir_rel(release);
    std::fs::create_dir_all(&cache_dir)?;
    std::fs::create_dir_all(&build_dir)?;

    eprintln!(
        "{}",
        color::info(&format!("building {} ({})", name, project_type))
    );

    let std_path_str = std_include_dir.to_string_lossy();

    // Build dependency graph while collecting source files
    let mut visited = HashSet::new();
    let mut graph = DependencyGraph::new();
    collect_imports_with_graph(
        &frontend,
        &frontend_work_dir,
        entry_file,
        &mut visited,
        &cache_dir,
        &std_path_str,
        &mut graph,
        &name,
    )?;

    let mut files: Vec<String> = Vec::new();
    let std_str = std_include_dir.join("std/src/str.miva");
    if std_str.exists() {
        files.push(std_str.to_string_lossy().to_string());
    }
    files.push(entry_file.to_string());
    for file in &visited {
        if !files.contains(file) {
            files.push(file.clone());
        }
    }

    // Phase 0: Collect macro definitions from all files (cross-file availability)
    let macro_table = collect_all_macros(&frontend, &frontend_work_dir, &files);
    let macro_count = macro_table.len();
    if macro_count > 0 {
        eprintln!(
            "{}",
            color::step(
                "macros",
                &format!("{} custom macro(s) collected", macro_count)
            )
        );
    }

    // Phase 1: Compile each .miva to .cpp (content-hash based caching)
    let mut cpp_results: Vec<(String, PathBuf, bool)> = Vec::new();
    let mut recompiled_files: HashSet<String> = HashSet::new();

    for file in &files {
        eprintln!("{}", color::step("compile", file));

        let cache_key = make_cache_key(file, &std_path_str);
        let was_cached = !needs_rebuild_by_hash(file, &cache_dir, &cache_key);

        let (cpp_path, has_main) = compile_file_to_cpp(
            &frontend,
            &frontend_work_dir,
            file,
            &cache_dir,
            &std_path_str,
            verbose,
            &macro_table,
        )?;

        if !was_cached {
            recompiled_files.insert(file.clone());
        }

        cpp_results.push((file.clone(), cpp_path, has_main));
    }

    // Determine which files need .cpp -> .o compilation
    // A file needs re-linking if it was recompiled OR if any of its dependencies was recompiled
    let mut need_compile_bin: HashSet<String> = HashSet::new();
    for file in &recompiled_files {
        need_compile_bin.insert(file.clone());
        // All transitive dependents of a changed file also need re-linking
        for dependent in graph.get_all_dependents(file) {
            need_compile_bin.insert(dependent);
        }
    }

    // If nothing was recompiled, check if .o files exist for all cpp files
    let all_cached = need_compile_bin.is_empty();

    // Phase 2: Compile .cpp to .o
    let mut obj_files: Vec<PathBuf> = Vec::new();

    for (file, cpp_path, _) in &cpp_results {
        if all_cached {
            // All files cached, use mtime check for .o
            let obj_path = cpp_path.with_extension("o");
            if obj_path.exists() {
                obj_files.push(obj_path);
                continue;
            }
        } else if !need_compile_bin.contains(file) {
            // This file wasn't recompiled and isn't a dependent of a changed file
            let obj_path = cpp_path.with_extension("o");
            if obj_path.exists() {
                obj_files.push(obj_path);
                continue;
            }
        }

        let obj_path = compile_cpp_to_obj(
            cpp_path,
            &cache_dir,
            &std_include_dir,
            &build_dir,
            project_type,
            release,
            verbose,
        )?;

        obj_files.push(obj_path);
    }

    eprintln!("{}", color::step("link", name));

    let _ = link_objects(&obj_files, name, &build_dir, project_type, release)?;

    if !env::get_keep_cpp() {
        for obj in &obj_files {
            let cpp = obj.with_extension("cpp");
            let _ = std::fs::remove_file(cpp);
        }
    }

    if project_type == "lib" {
        let lib_header = cache_dir.join("src/lib.h");
        if lib_header.exists() {
            let dest = build_dir.join(format!("{}.h", name));
            std::fs::copy(&lib_header, &dest)?;
        }
    }

    println!("{}", color::success("compilation finished"),);

    Ok(())
}
