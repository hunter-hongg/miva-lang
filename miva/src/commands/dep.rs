use std::collections::{HashMap, HashSet};
use std::path::Path;

use anyhow::Result;

use super::{color, env, frontend, lock};
use crate::config::Config;
use crate::json_ast;

pub fn exec(_verbose: bool) -> Result<()> {
    let config = Config::load().ok_or_else(|| anyhow::anyhow!("no miva.toml found"))?;
    let declared = config.dependencies();

    // Phase 1: resolve external dependencies (lock file)
    if !declared.is_empty() {
        let std_include = env::get_std_include_dir();
        let resolved = lock::resolve_force(&declared, &std_include)?;
        lock::write_lock(&resolved)?;
        for (name, version) in &resolved {
            eprintln!("  {} {} ({})", color::info("✓"), name, version);
        }
        eprintln!("{}", color::success("dependencies resolved — lock file written"));
    }

    let deps = if declared.is_empty() {
        HashMap::new()
    } else {
        lock::read_lock().unwrap_or_default()
    };

    // Phase 2: build and display file dependency graph
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

    let entry = if project_type == "lib" {
        "src/lib.miva"
    } else {
        "src/main.miva"
    };

    if !Path::new(entry).exists() {
        anyhow::bail!("entry file not found: {}", entry);
    }

    let (frontend, work_dir) =
        frontend::find_frontend().ok_or_else(|| anyhow::anyhow!("miva-frontend not found"))?;

    let std_dir = env::get_std_include_dir();

    // resolved_path -> Vec<(resolved_child_path, original_import_path)>
    let mut graph: HashMap<String, Vec<(String, String)>> = HashMap::new();
    let mut visited = HashSet::new();

    build_graph(
        &frontend,
        &work_dir,
        entry,
        name,
        &deps,
        &std_dir,
        &mut graph,
        &mut visited,
    )?;

    println!();
    println!("{}", color::info("file dependency graph:"));
    let mut shown = HashSet::new();
    print_tree(&graph, entry, entry, "", true, true, &mut shown);

    Ok(())
}

/// Resolve an import path to a real filesystem path.
fn resolve_path(
    import: &str,
    name: &str,
    deps: &HashMap<String, String>,
    std_dir: &Path,
) -> String {
    if Path::new(import).exists() {
        return import.to_string();
    }
    let parts: Vec<&str> = import.split('/').collect();
    if parts.is_empty() || parts[0].is_empty() {
        return import.to_string();
    }
    let first = parts[0];
    let rest = parts[1..].join("/");
    if first == name {
        format!("src/{}.miva", rest)
    } else if deps.contains_key(first) {
        let ver = &deps[first];
        format!(
            "{}/{}-{}/src/{}.miva",
            std_dir.display(),
            first,
            ver,
            rest
        )
    } else {
        format!("{}/{}/src/{}.miva", std_dir.display(), first, rest)
    }
}

/// Recursively parse files and build the import graph.
fn build_graph(
    frontend: &str,
    work_dir: &Option<String>,
    file: &str,
    name: &str,
    deps: &HashMap<String, String>,
    std_dir: &Path,
    graph: &mut HashMap<String, Vec<(String, String)>>,
    visited: &mut HashSet<String>,
) -> Result<()> {
    let resolved = resolve_path(file, name, deps, std_dir);

    if !visited.insert(resolved.clone()) {
        return Ok(());
    }

    if !Path::new(&resolved).exists() {
        graph.insert(resolved, Vec::new());
        return Ok(());
    }

    let json_str = match frontend::run_frontend(frontend, work_dir, &resolved) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "{}",
                color::warn(&format!("failed to parse {}: {}", resolved, e))
            );
            graph.insert(resolved, vec![]);
            return Ok(());
        }
    };
    let ast = match json_ast::from_str(&json_str) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!(
                "{}",
                color::warn(&format!("invalid JSON AST from {}: {}", resolved, e))
            );
            graph.insert(resolved, vec![]);
            return Ok(());
        }
    };

    let imports: Vec<String> = ast
        .defs
        .iter()
        .filter_map(|def| match def {
            crate::ast::Def::SImport { path, .. }
            | crate::ast::Def::SImportAs { path, .. }
            | crate::ast::Def::SImportHere { path, .. } => Some(path.clone()),
            _ => None,
        })
        .collect();

    let children: Vec<(String, String)> = imports
        .iter()
        .map(|imp| {
            let r = resolve_path(imp, name, deps, std_dir);
            (r, imp.clone())
        })
        .collect();

    graph.insert(resolved.clone(), children);

    // Recurse into children — clone to avoid borrow conflict
    let owned = graph[&resolved].clone();
    for (child_resolved, child_import) in &owned {
        if visited.contains(child_resolved) {
            continue;
        }
        build_graph(
            frontend,
            work_dir,
            child_import,
            name,
            deps,
            std_dir,
            graph,
            visited,
        )?;
    }

    Ok(())
}

/// Format a path for user-friendly display.
fn display_path(resolved: &str, orig: &str) -> String {
    if resolved.starts_with("src/") {
        resolved.to_string()
    } else {
        format!("[external] {}", orig)
    }
}

/// Recursively print the dependency tree with box-drawing characters.
fn print_tree(
    graph: &HashMap<String, Vec<(String, String)>>,
    node: &str,
    orig: &str,
    prefix: &str,
    is_last: bool,
    is_root: bool,
    shown: &mut HashSet<String>,
) {
    if is_root {
        println!("{}", display_path(node, orig));
    } else {
        let conn = if is_last { "└── " } else { "├── " };
        let label = if shown.contains(node) {
            format!("{} (already shown)", display_path(node, orig))
        } else {
            display_path(node, orig)
        };
        println!("{}{}{}", prefix, conn, label);
    }

    if !shown.insert(node.to_string()) {
        return;
    }

    let children = match graph.get(node) {
        Some(c) => c.clone(),
        None => return,
    };

    let child_prefix = if is_root {
        String::new()
    } else if is_last {
        format!("{}    ", prefix)
    } else {
        format!("{}│   ", prefix)
    };

    for (i, (child_path, child_import)) in children.iter().enumerate() {
        let last = i == children.len() - 1;

        if graph.contains_key(child_path.as_str()) {
            print_tree(
                graph,
                child_path,
                child_import,
                &child_prefix,
                last,
                false,
                shown,
            );
        } else {
            // Leaf node: external dep or file not found
            let conn = if last { "└── " } else { "├── " };
            let label = if shown.contains(child_path.as_str()) {
                format!("{} (already shown)", display_path(child_path, child_import))
            } else {
                display_path(child_path, child_import)
            };
            println!("{}{}{}", child_prefix, conn, label);
            shown.insert(child_path.clone());
        }
    }
}
