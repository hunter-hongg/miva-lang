use std::collections::{HashMap, HashSet};
use std::fs::exists;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::Result;

use super::{color, dependency_graph::DependencyGraph, env, frontend, lock};
use crate::ast::{Def, Param, Safety, Typ};
use crate::codegen;
use crate::codegen::Backend;
use crate::commands::clean;
use crate::config::Config;
use crate::error;
use crate::json_ast;
use crate::macro_expand;
use crate::magical;
use crate::semantic;
use crate::typecheck;
use crate::warning;

#[derive(clap::Args)]
pub struct Args {
    #[arg(short = 'b', long, help = "Backend to use: cxx (default) or llvm. Overrides miva.toml project.backend.")]
    pub backend: Option<String>,
}

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
    deps: &HashMap<String, String>,
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
            _ if deps.contains_key(&first.to_string()) => {
                let version = &deps[&first.to_string()];
                let inc = format!("{}", env::get_std_include_dir().display());
                file = inc + "/" + first + "-" + version + "/src/" + file_parts.join("/").as_str() + ".miva";
            }
            _ => {
                if !deps.is_empty() {
                    anyhow::bail!("import '{}' references library '{}' which is not declared in [dependencies]", _file, first);
                }
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
    let deps_list = read_dep_cache(&dep_path, file);

    let import_paths: Vec<String> = if let Some(deps_list) = deps_list {
        deps_list
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
            frontend, work_dir, path, visited, cache_dir, std_path, graph, name, deps,
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

fn needs_rebuild_by_hash(file: &str, cache_dir: &Path, cache_key: &str, backend: Backend) -> bool {
    let hash_path = env::hash_file_path(&cache_dir.to_path_buf(), cache_key);
    let current_hash = env::compute_sha256(file);

    if let Ok(stored) = std::fs::read_to_string(&hash_path) {
        let lines: Vec<&str> = stored.trim().lines().collect();
        if lines.len() >= 2 {
            // Format: line 1 = hash, line 2 = backend name
            lines[0].trim() != current_hash || lines[1].trim() != backend.name()
        } else {
            // Legacy format: just the hash
            lines[0].trim() != current_hash
        }
    } else {
        true
    }
}

fn update_hash_cache(file: &str, cache_dir: &Path, cache_key: &str, backend: Backend) {
    let hash_path = env::hash_file_path(&cache_dir.to_path_buf(), cache_key);
    let current_hash = env::compute_sha256(file);
    if let Some(parent) = hash_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::write(&hash_path, format!("{}\n{}", current_hash, backend.name()));
}

fn compile_file_to_src(
    frontend: &str,
    work_dir: &Option<String>,
    file: &str,
    cache_dir: &Path,
    std_path: &str,
    _verbose: bool,
    macro_table: &macro_expand::MacroTable,
    backend: Backend,
    func_sigs: &std::collections::HashMap<String, crate::codegen::FuncSig>,
    global_type_sigs: &std::collections::HashMap<String, (Vec<String>, Vec<Param>, Option<Typ>)>,
    global_safety: &std::collections::HashMap<String, Safety>,
) -> Result<(PathBuf, bool)> {
    let cache_key = make_cache_key(file, std_path);
    let ext = backend.extension();
    let src_path = cache_dir.join(format!("{}.{}", cache_key, ext));

    if src_path.exists() && !needs_rebuild_by_hash(file, cache_dir, &cache_key, backend) {
        let has_main = std::fs::read_to_string(&src_path)
            .map(|s| s.contains("mvp_own_main"))
            .unwrap_or(false);
        return Ok((src_path, has_main));
    }

    let json_str = frontend::run_frontend(frontend, work_dir, file)?;
    let ast = json_ast::from_str(&json_str)
        .map_err(|e| anyhow::anyhow!("Failed to parse JSON AST from {}: {}", file, e))?;

    let defs = macro_expand::expand_macros(&ast.defs, macro_table)?;

    let source = std::fs::read_to_string(file).unwrap_or_default();

    let sem_errors = semantic::check_program_with(&defs, global_safety);
    if !sem_errors.is_empty() {
        for err in &sem_errors {
            eprintln!(
                "{}",
                color::colorize(
                    color::RED,
                    error::format_error_with_source(err, file, &source).as_str()
                )
            );
        }
        anyhow::bail!("semantic errors found");
    }

    let type_errors = typecheck::check_program_with(&defs, global_type_sigs);
    if !type_errors.is_empty() {
        for err in &type_errors {
            eprintln!(
                "{}",
                color::colorize(
                    color::RED,
                    error::format_error_with_source(err, file, &source).as_str()
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
                warning::format_warning_with_source(w, file, &source).as_str()
            )
        );
    }
    if !err_warnings.is_empty() {
        anyhow::bail!("some warnings treated as errors");
    }
    for w in &warnings {
        eprintln!(
            "{}",
            color::colorize(
                color::YELLOW,
                warning::format_warning_with_source(w, file, &source).as_str()
            )
        );
    }

    // For MVM backend: collect defs, skip per-file code generation.
    // Combined compilation happens later in exec(). Do NOT update hash
    // cache here — that would trick needs_build into thinking the file
    // is up-to-date when bytecode was never generated.
    if backend == Backend::Mvm {
        return Ok((src_path, false));
    }

    let output = codegen::build_ir_with_backend(&defs, backend, func_sigs);

    std::fs::create_dir_all(src_path.parent().unwrap())?;
    std::fs::write(&src_path, &output.program)?;

    if !output.header.is_empty() {
        let header_path = cache_dir.join(format!("{}.h", cache_key));
        std::fs::write(&header_path, &output.header)?;
        // For LLVM backend, also write bridge file
        if backend == Backend::Llvm {
            let bridge_path = cache_dir.join(format!("{}.bridge.cpp", cache_key));
            std::fs::write(&bridge_path, &output.header)?;
        }
    }

    if !output.test.is_empty() {
        let test_ext = backend.extension();
        let test_path = cache_dir.join(format!("{}.test.{}", cache_key, test_ext));
        let test = if !output.header.is_empty() && backend == Backend::Cxx {
            let basename = std::path::Path::new(&cache_key)
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or(&cache_key);
            format!("#include \"{}.h\"\n{}", basename, output.test)
        } else {
            output.test
        };
        std::fs::write(&test_path, &test)?;
    }

    update_hash_cache(file, cache_dir, &cache_key, backend);

    let prog_str = String::from_utf8_lossy(&output.program);
    let has_main = prog_str.contains("mvp_own_main");
    Ok((src_path, has_main))
}

fn include_flag(dir: &[&Path]) -> Vec<String> {
    let mut flag = Vec::new();
    for i in dir {
        let f = format!("-I{}", i.to_str().unwrap());
        flag.push(f);
    }
    flag
}

fn compile_src_to_obj(
    src_path: &Path,
    cache_dir: &Path,
    std_include: &Path,
    build_dir: &Path,
    project_type: &str,
    release: bool,
    verbose: bool,
    extra_includes: &[PathBuf],
    backend: Backend,
) -> Result<PathBuf> {
    let obj_path = src_path.with_extension("o");

    match backend {
        Backend::Cxx => {
            let opt_flag = if release { "-O2" } else { "-g" };
            let pic_flag = if project_type == "lib" { "-fPIC" } else { "" };
            let inc_flags = env::get_include_flags();
            let mut include = include_flag(&[cache_dir, std_include, build_dir]);
            for extra in extra_includes {
                include.push(format!("-I{}", extra.to_string_lossy()));
            }

            let mut args = vec![
                opt_flag,
                "-std=c++20",
                "-c",
                src_path.to_str().unwrap(),
                "-o",
                obj_path.to_str().unwrap(),
                // Cross-module template bodies (e.g. vec.miva calling
                // mvp_std::mem::alloc inside a template) trip g++'s
                // -Wtemplate-body diagnostic even when the declaration IS
                // visible via #include. vec's headers emit the include, so
                // silence this pedantic warning.
                "-Wno-template-body",
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
                    for line in stderr.lines().take(5) {
                        eprintln!("{}", line);
                    }
                }
                if !env::get_keep_cpp() {
                    clean::exec(false)?;
                }
                std::process::exit(1);
            }

            Ok(obj_path)
        }
            Backend::Llvm => {
            let llc_output = Command::new("llc")
                .args([
                    "-filetype=obj",
                    src_path.to_str().unwrap(),
                    "-o",
                    obj_path.to_str().unwrap(),
                ])
                .output()
                .map_err(|e| anyhow::anyhow!("Failed to run llc: {}", e))?;

            if !llc_output.status.success() {
                let stderr = String::from_utf8_lossy(&llc_output.stderr);
                eprintln!("{}", color::error("llc compilation failed"));
                if verbose {
                    eprintln!("{}", stderr);
                } else {
                    for line in stderr.lines().take(5) {
                        eprintln!("{}", line);
                    }
                }
                if !env::get_keep_cpp() {
                    clean::exec(false)?;
                }
                std::process::exit(1);
            }

            Ok(obj_path)
        }
            Backend::Mvm => {
                anyhow::bail!("compile_src_to_obj should not be called for MVM backend");
            }
    }
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

pub fn exec(verbose: bool, release: bool, cli_backend: Option<String>) -> Result<()> {
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

    let backend_from_config = config.project_backend().unwrap_or_else(|| "cxx".to_string());
    let backend_name = cli_backend.unwrap_or(backend_from_config);
    let backend = Backend::from_name(&backend_name)
        .ok_or_else(|| anyhow::anyhow!("Unknown backend '{}'. Use 'cxx', 'llvm', or 'mvm'.", backend_name))?;

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
        color::info(&format!("building {} ({}) [backend: {}]", name, project_type, backend.name()))
    );

    let std_path_str = std_include_dir.to_string_lossy();

    // Resolve dependencies
    let declared = config.dependencies();
    let deps = if declared.is_empty() {
        HashMap::new()
    } else {
        lock::resolve(&declared, &std_include_dir)?
    };

    if !deps.is_empty() {
        eprintln!("  {}", color::info(&format!("dependencies: {}", deps.iter().map(|(n, v)| format!("{}={}", n, v)).collect::<Vec<_>>().join(", "))));
    }

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
        &deps,
    )?;

    let mut files: Vec<String> = Vec::new();
    let std_str = if let Some(std_ver) = deps.get("std") {
        let ver_dir = format!("std-{}", std_ver);
        std_include_dir.join(ver_dir).join("src/str.miva")
    } else {
        std_include_dir.join("std/src/str.miva")
    };
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

    // Phase 0.5: Collect function signatures from all files (cross-file type info)
    let mut all_func_sigs = std::collections::HashMap::new();
    // Qualified (module-prefixed) signatures used to resolve cross-module
    // calls during per-file type checking, e.g. `mvp_std.json.parse`.
    let mut global_type_sigs: std::collections::HashMap<
        String,
        (Vec<String>, Vec<Param>, Option<Typ>),
    > = std::collections::HashMap::new();
    // Qualified (module-prefixed) safety levels used to enforce the "cannot
    // call unsafe function from safe function" rule across module boundaries,
    // e.g. `mvp_std.json.as_string` -> unsafe. Mirrors `global_type_sigs`.
    let mut global_safety: std::collections::HashMap<String, Safety> =
        std::collections::HashMap::new();
    // Project name is used to qualify local (non-std) module calls the same
    // way the frontend does (see `util::process_call_path` / import paths).
    let pkg_name = name.clone();
    for file in &files {
        let json_str = frontend::run_frontend(&frontend, &frontend_work_dir, file)?;
        let ast = json_ast::from_str(&json_str)
            .map_err(|e| anyhow::anyhow!("Failed to parse JSON AST from {}: {}", file, e))?;
        let defs = macro_expand::expand_macros(&ast.defs, &macro_table)?;
        // Module name of this file (used to qualify its function signatures).
        let module_name = defs
            .iter()
            .find_map(|d| match d {
                crate::ast::Def::DModule { name, .. } => Some(name.clone()),
                _ => None,
            })
            .unwrap_or_default();
        for d in &defs {
            let func = match d {
                crate::ast::Def::DFunc { name, safety, .. } => Some((name, safety)),
                crate::ast::Def::DCFuncUnsafe { name, safety, .. } => Some((name, safety)),
                _ => None,
            };
            if let Some((name, safety)) = func {
                use std::collections::hash_map::Entry;
                match all_func_sigs.entry(name.clone()) {
                    Entry::Occupied(_) => {}
                    Entry::Vacant(v) => {
                        let (type_params, _params, returns, is_async) = match d {
                            crate::ast::Def::DFunc {
                                type_params,
                                params,
                                returns,
                                is_async,
                                ..
                            } => (
                                type_params.clone(),
                                params.clone(),
                                returns.clone(),
                                *is_async,
                            ),
                            _ => (Vec::new(), Vec::new(), None, false),
                        };
                        v.insert(crate::codegen::FuncSig {
                            type_params,
                            returns,
                            is_async,
                        });
                    }
                }
                // Qualified key mirrors the frontend's call-path rewriting
                // (`util::process_call_path`):
                //   - `std.x.y`      -> `mvp_std.x.y`
                //   - `main`         -> `main.y`
                //   - local module   -> `<pkg>.{path under src}.y`, e.g.
                //     `import "pkg/lib"` yields call `pkg.lib.y`.
                let qual_prefix: String = if module_name == "main" {
                    "main".to_string()
                } else if module_name.starts_with("std") {
                    format!("mvp_{}", module_name)
                } else {
                    let local = file
                        .strip_prefix("src/")
                        .unwrap_or(file.as_str())
                        .strip_suffix(".miva")
                        .unwrap_or(file.as_str())
                        .replace('/', ".");
                    format!("{}.{}", pkg_name, local)
                };
                let qual = format!("{}.{}", qual_prefix, name);
                global_type_sigs
                    .entry(qual.clone())
                    .or_insert_with(|| {
                        let (type_params, params, returns) = match d {
                            crate::ast::Def::DFunc {
                                type_params,
                                params,
                                returns,
                                ..
                            } => {
                                // Normalize generic types so cross-module
                                // signatures use TGenericParam (not TStruct)
                                // for type parameter names — otherwise typecheck
                                // cannot resolve `T` during instantiation.
                                let norm_params =
                                    typecheck::normalize_params(params, type_params);
                                let norm_returns = returns
                                    .as_ref()
                                    .map(|r| typecheck::normalize_typ(r, type_params));
                                (type_params.clone(), norm_params, norm_returns)
                            }
                            _ => (Vec::new(), Vec::new(), None),
                        };
                        (type_params, params, returns)
                    });
                global_safety.entry(qual).or_insert_with(|| safety.clone());
            }
        }
    }

    // Phase 1: Compile each .miva to source file (content-hash based caching)
    let mut src_results: Vec<(String, PathBuf, bool)> = Vec::new();
    let mut recompiled_files: HashSet<String> = HashSet::new();

    for file in &files {
        eprintln!("{}", color::step("compile", file));

        let cache_key = make_cache_key(file, &std_path_str);
        let was_cached = !needs_rebuild_by_hash(file, &cache_dir, &cache_key, backend);

        let (src_path, has_main) = compile_file_to_src(
            &frontend,
            &frontend_work_dir,
            file,
            &cache_dir,
            &std_path_str,
            verbose,
            &macro_table,
            backend,
            &all_func_sigs,
            &global_type_sigs,
            &global_safety,
        )?;

        if !was_cached {
            recompiled_files.insert(file.clone());
        }

        src_results.push((file.clone(), src_path, has_main));
    }

    // Determine which files need source -> .o compilation
    let mut need_compile_bin: HashSet<String> = HashSet::new();
    for file in &recompiled_files {
        need_compile_bin.insert(file.clone());
        for dependent in graph.get_all_dependents(file) {
            need_compile_bin.insert(dependent);
        }
    }

    let all_cached = need_compile_bin.is_empty();

    // Create cache symlinks for versioned dep includes
    for (dep_name, dep_ver) in &deps {
        let versioned_cache = cache_dir.join(format!("{}-{}", dep_name, dep_ver));
        let unversioned_link = cache_dir.join(dep_name);
        if versioned_cache.exists() && !unversioned_link.exists() {
            std::os::unix::fs::symlink(
                versioned_cache.strip_prefix(&cache_dir).unwrap_or(&versioned_cache),
                &unversioned_link,
            )?;
        }
    }

    // MVM backend: compile all defs to a single .mvm file
    if backend == Backend::Mvm {
        let mvm_path = if project_type == "lib" {
            build_dir.join(format!("lib{}.mvm", name))
        } else {
            build_dir.join(format!("{}.mvm", name))
        };

        // Check if bytecode needs rebuild (if any source changed)
        let needs_build = if mvm_path.exists() {
            let all_files_up_to_date = files.iter().all(|f| {
                let cache_key = make_cache_key(f, &std_path_str);
                !needs_rebuild_by_hash(f, &cache_dir, &cache_key, backend)
            });
            !all_files_up_to_date
        } else {
            true
        };

        if needs_build {
            eprintln!("{}", color::step("compile", "mvm bytecode"));

            // Collect all expanded defs from all files
            let mut all_defs: Vec<Def> = Vec::new();
            for file in &files {
                let json_str = frontend::run_frontend(&frontend, &frontend_work_dir, file)?;
                let ast = json_ast::from_str(&json_str)
                    .map_err(|e| anyhow::anyhow!("Failed to parse JSON AST from {}: {}", file, e))?;
                let defs = macro_expand::expand_macros(&ast.defs, &macro_table)?;
                all_defs.extend(defs);
            }

            // Generate combined MVM bytecode
            let output = codegen::build_ir_with_backend(&all_defs, backend, &all_func_sigs);

            // Write .mvm file
            std::fs::create_dir_all(&build_dir)?;
            std::fs::write(&mvm_path, &output.program)?;

            // Update hash cache for all files
            for file in &files {
                let cache_key = make_cache_key(file, &std_path_str);
                update_hash_cache(file, &cache_dir, &cache_key, backend);
            }

            eprintln!("{}", color::success(&format!("{} -> {}", name, mvm_path.display())));
        }

        println!("{}", color::success("compilation finished"));
        return Ok(());
    }

    // Phase 2: Compile source to .o
    let mut obj_files: Vec<PathBuf> = Vec::new();
    let mut dep_include_dirs: Vec<PathBuf> = Vec::new();
    for (dep_name, dep_ver) in &deps {
        dep_include_dirs.push(std_include_dir.join(format!("{}-{}", dep_name, dep_ver)));
    }

    let mut bridge_compiled = false;
    for (_i, (file, src_path, _)) in src_results.iter().enumerate() {
        if all_cached {
            let obj_path = src_path.with_extension("o");
            if obj_path.exists() {
                obj_files.push(obj_path);
                continue;
            }
        } else if !need_compile_bin.contains(file) {
            let obj_path = src_path.with_extension("o");
            if obj_path.exists() {
                obj_files.push(obj_path);
                continue;
            }
        }

        let obj_path = compile_src_to_obj(
            src_path,
            &cache_dir,
            &std_include_dir,
            &build_dir,
            project_type,
            release,
            verbose,
            &dep_include_dirs,
            backend,
        )?;
        if backend == Backend::Llvm && !bridge_compiled {
            let bridge_src = src_path.with_extension("bridge.cpp");
            let bridge_obj = src_path.with_extension("bridge.o");
            if bridge_src.exists() {
                let opt_flag = if release { "-O2" } else { "-g" };
                let inc_flags = env::get_include_flags();
                let mut include = include_flag(&[&cache_dir, &std_include_dir, &build_dir]);
                for extra in &dep_include_dirs {
                    include.push(format!("-I{}", extra.to_string_lossy()));
                }
                let mut args: Vec<String> = vec![opt_flag.to_string(), "-std=c++20".into(), "-c".into()];
                args.push(bridge_src.to_str().unwrap().to_string());
                args.push("-o".into());
                args.push(bridge_obj.to_str().unwrap().to_string());
                for flag in inc_flags.split_whitespace() {
                    if !flag.is_empty() {
                        args.push(flag.to_string());
                    }
                }
                for flag in &include {
                    args.push(flag.clone());
                }
                let bridge_output = Command::new("g++")
                    .args(&args)
                    .output()
                    .map_err(|e| anyhow::anyhow!("Failed to compile bridge: {}", e))?;
                if !bridge_output.status.success() {
                    let stderr = String::from_utf8_lossy(&bridge_output.stderr);
                    eprintln!("{}", color::error(&format!("bridge compilation failed:\n{}", stderr)));
                    if verbose {
                        eprintln!("{:?}", args);
                    }
                    std::process::exit(1);
                }
                obj_files.push(bridge_obj);
                bridge_compiled = true;
            }
        }

        obj_files.push(obj_path);
    }

    // Fallback: compile bridge if skipped due to caching (LLVM backend)
    if backend == Backend::Llvm && !bridge_compiled {
        for (_i, (file, src_path, _)) in src_results.iter().enumerate() {
            let bridge_src = src_path.with_extension("bridge.cpp");
            let bridge_obj = src_path.with_extension("bridge.o");
            if bridge_src.exists() {
                if !bridge_obj.exists() {
                    let opt_flag = if release { "-O2" } else { "-g" };
                    let inc_flags = env::get_include_flags();
                    let mut include = include_flag(&[&cache_dir, &std_include_dir, &build_dir]);
                    for extra in &dep_include_dirs {
                        include.push(format!("-I{}", extra.to_string_lossy()));
                    }
                    let mut args: Vec<String> = vec![opt_flag.to_string(), "-std=c++20".into(), "-c".into()];
                    args.push(bridge_src.to_str().unwrap().to_string());
                    args.push("-o".into());
                    args.push(bridge_obj.to_str().unwrap().to_string());
                    for flag in inc_flags.split_whitespace() {
                        if !flag.is_empty() {
                            args.push(flag.to_string());
                        }
                    }
                    for flag in &include {
                        args.push(flag.clone());
                    }
                    let bridge_output = Command::new("g++")
                        .args(&args)
                        .output()
                        .map_err(|e| anyhow::anyhow!("Failed to compile bridge: {}", e))?;
                    if !bridge_output.status.success() {
                        let stderr = String::from_utf8_lossy(&bridge_output.stderr);
                        eprintln!("{}", color::error(&format!("bridge compilation failed:\n{}", stderr)));
                        if verbose {
                            eprintln!("{:?}", args);
                        }
                        std::process::exit(1);
                    }
                }
                obj_files.push(bridge_obj);
                bridge_compiled = true;
                break;
            }
        }
    }

    obj_files.sort();
    obj_files.dedup();

    eprintln!("{}", color::step("link", name));

    let _ = link_objects(&obj_files, name, &build_dir, project_type, release)?;

    if !env::get_keep_cpp() {
        for obj in &obj_files {
            let src = obj.with_extension(backend.extension());
            let _ = std::fs::remove_file(src);
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
