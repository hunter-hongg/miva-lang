#![allow(dead_code)]

use crate::ast::*;
use crate::symbol_table::SymbolTable;

/// A compiler warning with code and human-readable message.
#[derive(Debug, Clone, PartialEq)]
pub struct Warning {
    pub code: String,
    pub message: String,
    pub loc: Loc,
}

impl Warning {
    fn new(code: &str, loc: &Loc, msg: &str) -> Self {
        Warning {
            code: code.to_string(),
            message: msg.to_string(),
            loc: loc.clone(),
        }
    }
}

/// Format a compiler warning in Rust-style with source code context.
pub fn format_warning_with_source(warn: &Warning, file_path: &str, source: &str) -> String {
    let mut output = String::new();
    output.push_str(&format!("warning[{}]: {}\n", warn.code, warn.message));

    let line = warn.loc.line;
    let col = warn.loc.col;

    if line > 0 {
        output.push_str(&format!(" --> {}:{}:{}\n", file_path, line, col));
        output.push_str("     |\n");

        let line_idx = (line - 1) as usize;
        if let Some(source_line) = source.lines().nth(line_idx) {
            output.push_str(&format!("{:>4} | {}\n", line, source_line));
            let caret_col = (col - 1).max(0) as usize;
            let caret_pos = caret_col.min(source_line.len());
            output.push_str(&format!("     | {}{}\n", " ".repeat(caret_pos), "^"));
        }
    }

    output
}

// ---------------------------------------------------------------------------
// Naming convention helpers (ported from miva-raw/lib/util.ml)
// ---------------------------------------------------------------------------

fn is_uppercase(c: char) -> bool {
    c.is_ascii_uppercase()
}

fn is_lowercase(c: char) -> bool {
    c.is_ascii_lowercase()
}

fn is_lowercase_or_dot(c: char) -> bool {
    is_lowercase(c) || c == '.'
}

fn check_snake(loc: &Loc, name: &str, typ: &str) -> Option<Warning> {
    if name.chars().any(is_uppercase) {
        Some(Warning::new(
            "W0001",
            loc,
            &format!("The {} name '{}' isn't a snake_case name.", typ, name),
        ))
    } else {
        None
    }
}

fn check_all_lower(loc: &Loc, name: &str, typ: &str) -> Option<Warning> {
    if name.chars().any(|c| !is_lowercase_or_dot(c)) {
        Some(Warning::new(
            "W0001",
            loc,
            &format!("The {} name '{}' isn't a lowercase name.", typ, name),
        ))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Deprecation checks (ported from miva-raw/lib/global.ml)
// ---------------------------------------------------------------------------

fn deprecated_func(name: &str, modname: &str) -> Option<String> {
    let msg = |dep: &str, replacement: &str, is_macro: bool| {
        let kind = if is_macro { "macro" } else { "function" };
        format!(
            "\"{}\" is deprecated, use {} \"{}\" instead",
            dep, kind, replacement
        )
    };
    let notrec = |dep: &str, replacement: &str, is_macro: bool| {
        let kind = if is_macro { "macro" } else { "function" };
        format!(
            "\"{}\" is not recommended, use {} \"{}\" instead",
            dep, kind, replacement
        )
    };
    let msg_if = |dep: &str, replacement: &str, is_macro: bool, exclude: &str| {
        if modname == exclude {
            None
        } else {
            Some(msg(dep, replacement, is_macro))
        }
    };
    let notrec_if = |dep: &str, replacement: &str, is_macro: bool, exclude: &str| {
        if modname == exclude {
            None
        } else {
            Some(notrec(dep, replacement, is_macro))
        }
    };

    match name {
        "prints" => Some(msg("prints", "prints", true)),
        "printlns" => Some(msg("printlns", "printlns", true)),
        "string_concat" => msg_if("string_concat", "std.str.concat", false, "std.str"),
        "string_parse" => msg_if("string_parse", "std.str.parse_int", false, "std.str"),
        "string_length" => msg_if("string_length", "std.str.len", false, "std.str"),
        "string_make" => msg_if("string_make", "std.str.make", false, "std.str"),
        "ptr_alloc" => notrec_if("alloc", "std.mem.alloc", false, "std.mem"),
        "ptr_realloc" => notrec_if("realloc", "std.mem.realloc", false, "std.mem"),
        "ptr_free" => notrec_if("free", "std.mem.free", false, "std.mem"),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Deprecated keyword checks
// ---------------------------------------------------------------------------

fn deprecated_keyword() -> &'static str {
    "\"c\" is a deprecated keyword, use \"inline\" instead"
}

// ---------------------------------------------------------------------------
// Core warning generation (ported from miva-raw/lib/warnings.ml)
// ---------------------------------------------------------------------------

fn check_expr(expr: &Expr, modname: &str, warnings: &mut Vec<Warning>) {
    match expr {
        Expr::ECall {
            loc,
            name,
            type_args: _,
            args,
        } => {
            if let Some(msg) = deprecated_func(name, modname) {
                warnings.push(Warning::new("W0002", loc, &msg));
            }
            for arg in args {
                check_expr(arg, modname, warnings);
            }
        }
        Expr::EMacro { .. } | Expr::EMacroVar { .. } => {
            // No-op: macros/macro-vars are skipped
        }
        Expr::EBinOp { left, right, .. } => {
            check_expr(left, modname, warnings);
            check_expr(right, modname, warnings);
        }
        Expr::EIf {
            cond, then, else_, ..
        } => {
            check_expr(cond, modname, warnings);
            check_expr(then, modname, warnings);
            if let Some(e) = else_ {
                check_expr(e, modname, warnings);
            }
        }
        Expr::EChoose {
            var,
            cases,
            otherwise,
            ..
        } => {
            check_expr(var, modname, warnings);
            for case in cases {
                check_expr(&case.when, modname, warnings);
                check_expr(&case.then, modname, warnings);
            }
            if let Some(e) = otherwise {
                check_expr(e, modname, warnings);
            }
        }
        Expr::EFieldAccess { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Expr::EStructLit { fields, .. } => {
            for vf in fields {
                check_expr(&vf.value, modname, warnings);
            }
        }
        Expr::EBlock { stmts, result, .. } => {
            for stmt in stmts {
                check_stmt(stmt, modname, warnings);
            }
            if let Some(e) = result {
                check_expr(e, modname, warnings);
            }
        }
        Expr::EArrayLit { values, .. } => {
            for elem in values {
                check_expr(elem, modname, warnings);
            }
        }
        Expr::ECast { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Expr::EWhile { cond, body, .. } => {
            check_expr(cond, modname, warnings);
            check_expr(body, modname, warnings);
        }
        Expr::ELoop { body, .. } => {
            check_expr(body, modname, warnings);
        }
        Expr::EFor { range, body, .. } => {
            check_expr(range, modname, warnings);
            check_expr(body, modname, warnings);
        }
        Expr::EAddr { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Expr::EDeref { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Expr::EInt { .. }
        | Expr::EFloat { .. }
        | Expr::EString { .. }
        | Expr::EBool { .. }
        | Expr::EChar { .. }
        | Expr::EVoid { .. }
        | Expr::EClone { .. }
        | Expr::EMove { .. }
        | Expr::EVar { .. } => {}
        Expr::EMethodCall { .. } => unreachable!(),
    }
}

fn check_stmt(stmt: &Stmt, modname: &str, warnings: &mut Vec<Warning>) {
    match stmt {
        Stmt::SLet {
            loc, name, expr, ..
        } => {
            if let Some(w) = check_snake(loc, name, "var") {
                warnings.push(w);
            }
            check_expr(expr, modname, warnings);
        }
        Stmt::SLetTyped {
            loc, name, expr, ..
        } => {
            if let Some(w) = check_snake(loc, name, "var") {
                warnings.push(w);
            }
            check_expr(expr, modname, warnings);
        }
        Stmt::SAssign { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Stmt::SFieldAssign { target, expr, .. } => {
            check_expr(target, modname, warnings);
            check_expr(expr, modname, warnings);
        }
        Stmt::SReturn { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Stmt::SExpr { expr, .. } => {
            check_expr(expr, modname, warnings);
        }
        Stmt::SCIntro { loc, content } => {
            // Bug-for-bug compat: the OCaml original always warns for SCIntro
            let s = content.trim();
            let sd: Vec<&str> = s.split(':').collect();
            if sd.len() < 2 {
                warnings.push(Warning::new("W0003", loc, "intro comments isn't valid"));
            } else {
                warnings.push(Warning::new(
                    "W0003",
                    loc,
                    &format!("invalid intro comment {}", sd[0]),
                ));
            }
        }
        Stmt::SEmpty { .. } => {}
    }
}

// ---------------------------------------------------------------------------
// Annotation-type checking (ported from miva-raw/lib/anoncheck.ml)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum AnonTyp {
    Impl,
    Unsafe,
    Trusted,
    Usage,
    Param,
    Invalid,
}

/// Parse the annotation type from the content of a DCIntro.
///
/// "unsafe: raw memory op" → AnonTyp::Unsafe
/// "  usage  : used in main" → AnonTyp::Usage (whitespace trimmed)
/// "just_text" → AnonTyp::Invalid (no colon, whole string treated as type)
fn typ_of_anon(s: &str) -> AnonTyp {
    let s = s.trim();
    let typ = s.split(':').next().unwrap_or("").trim();
    match typ {
        "impl" => AnonTyp::Impl,
        "unsafe" => AnonTyp::Unsafe,
        "trusted" => AnonTyp::Trusted,
        "usage" => AnonTyp::Usage,
        "param" => AnonTyp::Param,
        _ => AnonTyp::Invalid,
    }
}

/// Check that DCIntro annotations have valid types for the following definition.
///
/// Ported from `Anoncheck.check_anon` in `miva-raw/lib/anoncheck.ml`.
fn check_annotations(defs: &[Def]) -> Vec<Warning> {
    use AnonTyp::*;
    let mut warnings = Vec::new();
    let mut prev: Option<&Def> = None;

    for cur in defs {
        let prev_dcintro = match prev {
            Some(Def::DCIntro { content, .. }) => Some(content.as_str()),
            _ => None,
        };

        if let Some(anno_str) = prev_dcintro {
            let anno = typ_of_anon(anno_str);
            let loc = match cur {
                Def::DFunc { loc, .. }
                | Def::DCFuncUnsafe { loc, .. }
                | Def::DTest { loc, .. }
                | Def::DStruct { loc, .. }
                | Def::DModule { loc, .. }
                | Def::SExport { loc, .. }
                | Def::SImport { loc, .. }
                | Def::SImportAs { loc, .. }
                | Def::SImportHere { loc, .. }
                | Def::DImpl { loc, .. }
                | Def::DMacro { loc, .. }
                | Def::DEnum { loc, .. } => loc,
                // DCMagical and DCIntro don't need annotations
                Def::DCMagical { .. } | Def::DCIntro { .. } => {
                    prev = Some(cur);
                    continue;
                }
            };

            let valid = match cur {
                Def::DFunc { safety, .. } => match safety {
                    Safety::Safe => matches!(anno, Usage | Param),
                    Safety::Unsafe => matches!(anno, Unsafe | Usage | Param),
                    Safety::Trusted => matches!(anno, Trusted | Usage | Param),
                },
                Def::DCFuncUnsafe { .. } => matches!(anno, Unsafe | Usage | Param),
                Def::DTest { .. } => matches!(anno, Usage),
                Def::DStruct { .. } => matches!(anno, Usage | Impl),
                // Module/import/export/impl always warn
                Def::DModule { .. }
                | Def::SExport { .. }
                | Def::SImport { .. }
                | Def::SImportAs { .. }
                | Def::SImportHere { .. }
                | Def::DImpl { .. }
                | Def::DMacro { .. }
                | Def::DEnum { .. } => false,
                // DCMagical/DCIntro already handled above (skipped)
                Def::DCMagical { .. } | Def::DCIntro { .. } => unreachable!(),
            };

            if !valid {
                warnings.push(Warning::new("W0003", loc, "invalid intro comment type"));
            }
        }

        prev = Some(cur);
    }

    warnings
}

/// Collect all warnings from a list of top-level definitions.
///
/// This is the main entry point, mirroring `Warnings.get_warnings` from the
/// OCaml `miva-raw/lib/warnings.ml`.
pub fn get_warnings(defs: &[Def]) -> Vec<Warning> {
    let symt = SymbolTable::build(defs);
    let modname = &symt.module_name;
    let mut warnings = Vec::new();

    for def in defs {
        match def {
            Def::DFunc {
                loc, name, body, ..
            } => {
                if let Some(w) = check_snake(loc, name, "function") {
                    warnings.push(w);
                }
                check_expr(body, modname, &mut warnings);
            }
            Def::DModule { loc, name } => {
                if let Some(w) = check_all_lower(loc, name, "module") {
                    warnings.push(w);
                }
            }
            Def::DCFuncUnsafe {
                loc,
                used_c_keyword: true,
                ..
            } => {
                warnings.push(Warning::new("W0004", loc, deprecated_keyword()));
            }
            Def::DStruct { .. }
            | Def::DTest { .. }
            | Def::DCFuncUnsafe { .. }
            | Def::SExport { .. }
            | Def::SImport { .. }
            | Def::SImportAs { .. }
            | Def::SImportHere { .. }
            | Def::DCMagical { .. }
            | Def::DCIntro { .. }
            | Def::DImpl { .. }
            | Def::DMacro { .. }
            | Def::DEnum { .. } => {}
        }
    }

    let mut anno_warnings = check_annotations(defs);
    warnings.append(&mut anno_warnings);

    warnings
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Loc {
        Loc { line: 1, col: 1 }
    }

    fn make_module(name: &str) -> Def {
        Def::DModule {
            loc: loc(),
            name: name.to_string(),
        }
    }

    fn make_func(name: &str, body: Expr, safety: Safety) -> Def {
        Def::DFunc {
            loc: loc(),
            name: name.to_string(),
            type_params: vec![],
            params: Vec::new(),
            returns: None,
            body: Box::new(body),
            safety,
            is_async: false,
        }
    }

    fn make_func_loc(l: Loc, name: &str, body: Expr, safety: Safety) -> Def {
        Def::DFunc {
            loc: l.clone(),
            name: name.to_string(),
            type_params: vec![],
            params: Vec::new(),
            returns: None,
            body: Box::new(body),
            safety,
            is_async: false,
        }
    }

    // ------------------------------------------------------------------
    // Empty / no-warning cases
    // ------------------------------------------------------------------

    #[test]
    fn test_empty_defs_no_warnings() {
        let warns = get_warnings(&[]);
        assert!(warns.is_empty());
    }

    #[test]
    fn test_valid_program_no_warnings() {
        let defs = vec![
            make_module("test"),
            make_func("main", Expr::EVoid { loc: loc() }, Safety::Safe),
        ];
        let warns = get_warnings(&defs);
        assert!(warns.is_empty(), "Expected no warnings, got: {:?}", warns);
    }

    // ------------------------------------------------------------------
    // W0001 – snake_case naming
    // ------------------------------------------------------------------

    #[test]
    fn test_w0001_function_name_not_snake_case() {
        let defs = vec![
            make_module("test"),
            make_func("BadName", Expr::EVoid { loc: loc() }, Safety::Safe),
        ];
        let warns = get_warnings(&defs);
        assert_eq!(warns.len(), 1);
        assert_eq!(warns[0].code, "W0001");
        assert!(warns[0].message.contains("BadName"));
        assert!(warns[0].message.contains("function"));
    }

    #[test]
    fn test_w0001_var_name_not_snake_case() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SLet {
                        loc: loc(),
                        mutable: false,
                        name: "BadVar".to_string(),
                        expr: Box::new(Expr::EInt {
                            loc: loc(),
                            value: 1,
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        assert_eq!(warns.len(), 1);
        assert_eq!(warns[0].code, "W0001");
        assert!(warns[0].message.contains("BadVar"));
        assert!(warns[0].message.contains("var"));
    }

    #[test]
    fn test_w0001_module_name_not_lowercase() {
        let defs = vec![make_module("My.Mod")];
        let warns = get_warnings(&defs);
        assert_eq!(warns.len(), 1);
        assert_eq!(warns[0].code, "W0001");
        assert!(warns[0].message.contains("module"));
    }

    #[test]
    fn test_w0001_module_name_with_uppercase() {
        let warns = get_warnings(&[make_module("Std.IO")]);
        assert!(!warns.is_empty());
        assert_eq!(warns[0].code, "W0001");
    }

    #[test]
    fn test_module_name_with_dots_and_lowercase_is_valid() {
        let warns = get_warnings(&[make_module("std.io.utils")]);
        assert!(
            warns.is_empty(),
            "std.io.utils should be valid: {:?}",
            warns
        );
    }

    #[test]
    fn test_snake_case_function_name_is_valid() {
        let defs = vec![
            make_module("test"),
            make_func("my_function", Expr::EVoid { loc: loc() }, Safety::Safe),
        ];
        let warns = get_warnings(&defs);
        let snake_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0001").collect();
        assert!(
            snake_warns.is_empty(),
            "my_function should pass snake check: {:?}",
            snake_warns
        );
    }

    #[test]
    fn test_snake_case_var_name_is_valid() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SLet {
                        loc: loc(),
                        mutable: false,
                        name: "my_var".to_string(),
                        expr: Box::new(Expr::EInt {
                            loc: loc(),
                            value: 42,
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let snake_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0001").collect();
        assert!(
            snake_warns.is_empty(),
            "my_var should pass snake check: {:?}",
            snake_warns
        );
    }

    // ------------------------------------------------------------------
    // W0002 – deprecated function calls
    // ------------------------------------------------------------------

    #[test]
    fn test_w0002_deprecated_prints_call() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "prints".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        assert!(!warns.is_empty());
        assert_eq!(warns[0].code, "W0002");
        assert!(warns[0].message.contains("prints"));
        assert!(warns[0].message.contains("deprecated"));
    }

    #[test]
    fn test_w0002_deprecated_string_concat_call() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "string_concat".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        assert!(!warns.is_empty());
        assert_eq!(warns[0].code, "W0002");
        assert!(warns[0].message.contains("string_concat"));
    }

    #[test]
    fn test_no_w0002_for_deprecated_in_own_module() {
        // When inside `std.str`, string_concat should not warn
        let defs = vec![
            make_module("std.str"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "string_concat".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert!(
            dep_warns.is_empty(),
            "Should not warn inside std.str: {:?}",
            dep_warns
        );
    }

    #[test]
    fn test_w0002_ptr_alloc_not_recommended() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "ptr_alloc".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        assert!(!warns.is_empty());
        let ptr_warn = warns.iter().find(|w| w.code == "W0002").unwrap();
        assert!(ptr_warn.message.contains("not recommended"));
    }

    #[test]
    fn test_no_w0002_for_ptr_alloc_in_std_mem() {
        let defs = vec![
            make_module("std.mem"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "ptr_alloc".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let ptr_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert!(
            ptr_warns.is_empty(),
            "Should not warn inside std.mem: {:?}",
            ptr_warns
        );
    }

    #[test]
    fn test_no_w0002_for_normal_function_call() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "print".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert!(
            dep_warns.is_empty(),
            "print should not trigger W0002: {:?}",
            dep_warns
        );
    }

    #[test]
    fn test_w0002_in_nested_call_args() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "print".to_string(),
                    type_args: vec![],
                    args: vec![Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        // Only the nested 'prints' should trigger W0002
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
        assert!(dep_warns[0].message.contains("prints"));
    }

    // ------------------------------------------------------------------
    // W0003 – intro comments (bug-for-bug: always warns)
    // ------------------------------------------------------------------

    #[test]
    fn test_w0003_cintro_always_warns() {
        let defs = vec![
            make_module("test"),
            Def::DCIntro {
                loc: loc(),
                content: "intro comment".to_string(),
            },
        ];
        let warns = get_warnings(&defs);
        // Note: DCIntro is NOT processed in the def loop (catch-all _ => ())
        // So no warning expected here. SCIntro inside a block is what triggers it.
        assert!(warns.is_empty());
    }

    #[test]
    fn test_w0003_scintro_in_block_warns() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SCIntro {
                        loc: loc(),
                        content: "impl: some comment".to_string(),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        // SCIntro always warns (bug-for-bug)
        let cintro_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0003").collect();
        assert_eq!(cintro_warns.len(), 1);
    }

    #[test]
    fn test_w0003_scintro_short_format() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SCIntro {
                        loc: loc(),
                        content: "no_colon".to_string(),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let cintro_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0003").collect();
        assert_eq!(cintro_warns.len(), 1);
        assert!(cintro_warns[0].message.contains("isn't valid"));
    }

    // ------------------------------------------------------------------
    // Multiple warnings
    // ------------------------------------------------------------------

    #[test]
    fn test_multiple_warnings() {
        let defs = vec![
            make_module("Bad.Mod"),
            make_func("BadName", Expr::EVoid { loc: loc() }, Safety::Safe),
            make_func(
                "main",
                Expr::ECall {
                    loc: loc(),
                    name: "prints".to_string(),
                    type_args: vec![],
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        // 1 W0001 for Bad.Mod, 1 W0001 for BadName, 1 W0002 for prints
        assert_eq!(warns.len(), 3);
        assert_eq!(warns.iter().filter(|w| w.code == "W0001").count(), 2);
        assert_eq!(warns.iter().filter(|w| w.code == "W0002").count(), 1);
    }

    // ------------------------------------------------------------------
    // Expression traversal coverage
    // ------------------------------------------------------------------

    #[test]
    fn test_w0002_in_array_lit() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EArrayLit {
                    loc: loc(),
                    values: vec![Expr::ECall {
                        loc: loc(),
                        name: "string_concat".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_bin_op() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Add,
                    left: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                    right: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_if_expr() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    then: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                    else_: Some(Box::new(Expr::ECall {
                        loc: loc(),
                        name: "printlns".to_string(),
                        type_args: vec![],
                        args: vec![],
                    })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 2);
    }

    #[test]
    fn test_w0002_in_while_loop() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EWhile {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    body: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "string_concat".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_for_loop() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EFor {
                    loc: loc(),
                    var: "i".to_string(),
                    range: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                    body: Box::new(Expr::EVoid { loc: loc() }),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_cast() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ECast {
                    loc: loc(),
                    expr: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                    to: Typ::TInt,
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_field_access() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EFieldAccess {
                    loc: loc(),
                    expr: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                    field: "x".to_string(),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_addr() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EAddr {
                    loc: loc(),
                    expr: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_deref() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EDeref {
                    loc: loc(),
                    expr: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "prints".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_choose_expr() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EChoose {
                    loc: loc(),
                    var: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    cases: vec![WhenCase {
                        when: Box::new(Expr::EInt {
                            loc: loc(),
                            value: 1,
                        }),
                        then: Box::new(Expr::ECall {
                            loc: loc(),
                            name: "prints".to_string(),
                            type_args: vec![],
                            args: vec![],
                        }),
                    }],
                    otherwise: Some(Box::new(Expr::ECall {
                        loc: loc(),
                        name: "printlns".to_string(),
                        type_args: vec![],
                        args: vec![],
                    })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 2);
    }

    #[test]
    fn test_w0002_in_loop() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::ELoop {
                    loc: loc(),
                    body: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "string_concat".to_string(),
                        type_args: vec![],
                        args: vec![],
                    }),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_struct_lit_field() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EStructLit {
                    loc: loc(),
                    name: "Point".to_string(),
                    type_args: vec![],
                    fields: vec![ValueField {
                        name: "x".to_string(),
                        value: Expr::ECall {
                            loc: loc(),
                            name: "prints".to_string(),
                            type_args: vec![],
                            args: vec![],
                        },
                    }],
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_return_stmt() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SReturn {
                        loc: loc(),
                        expr: Box::new(Expr::ECall {
                            loc: loc(),
                            name: "prints".to_string(),
                            type_args: vec![],
                            args: vec![],
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_expr_stmt() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SExpr {
                        loc: loc(),
                        expr: Box::new(Expr::ECall {
                            loc: loc(),
                            name: "string_length".to_string(),
                            type_args: vec![],
                            args: vec![],
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    #[test]
    fn test_w0002_in_assign_stmt() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SAssign {
                        loc: loc(),
                        name: "x".to_string(),
                        expr: Box::new(Expr::ECall {
                            loc: loc(),
                            name: "prints".to_string(),
                            type_args: vec![],
                            args: vec![],
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
        assert_eq!(dep_warns.len(), 1);
    }

    // ------------------------------------------------------------------
    // Edge cases
    // ------------------------------------------------------------------

    #[test]
    fn test_w0002_all_deprecated_functions() {
        let deprecated = [
            "prints",
            "printlns",
            "string_concat",
            "string_parse",
            "string_length",
            "string_make",
            "ptr_alloc",
            "ptr_realloc",
            "ptr_free",
        ];
        for func_name in deprecated {
            let defs = vec![
                make_module("test"),
                make_func(
                    "main",
                    Expr::ECall {
                        loc: loc(),
                        name: func_name.to_string(),
                        type_args: vec![],
                        args: vec![],
                    },
                    Safety::Safe,
                ),
            ];
            let warns = get_warnings(&defs);
            let dep_warns: Vec<_> = warns.iter().filter(|w| w.code == "W0002").collect();
            assert!(
                !dep_warns.is_empty(),
                "Expected W0002 for '{}', got: {:?}",
                func_name,
                warns
            );
        }
    }

    #[test]
    fn test_unsafe_function_no_warning() {
        let defs = vec![
            make_module("test"),
            make_func("do_stuff", Expr::EVoid { loc: loc() }, Safety::Unsafe),
        ];
        let warns = get_warnings(&defs);
        assert!(warns.is_empty());
    }

    #[test]
    fn test_trusted_function_no_warning() {
        let defs = vec![
            make_module("test"),
            make_func("do_stuff", Expr::EVoid { loc: loc() }, Safety::Trusted),
        ];
        let warns = get_warnings(&defs);
        assert!(warns.is_empty());
    }

    #[test]
    fn test_dcfunccunsafe_skipped() {
        let defs = vec![
            make_module("test"),
            Def::DCFuncUnsafe {
                loc: loc(),
                name: "c_func".to_string(),
                params: Vec::new(),
                returns: None,
                code: "return 0;".to_string(),
                safety: Safety::Unsafe,
                used_c_keyword: false,
            },
        ];
        let warns = get_warnings(&defs);
        assert!(warns.is_empty());
    }

    #[test]
    fn test_w0001_loc_correct() {
        let specific_loc = Loc { line: 42, col: 7 };
        let defs = vec![
            make_module("test"),
            make_func_loc(
                specific_loc.clone(),
                "BadName",
                Expr::EVoid { loc: loc() },
                Safety::Safe,
            ),
        ];
        let warns = get_warnings(&defs);
        assert_eq!(warns[0].loc.line, 42);
        assert_eq!(warns[0].loc.col, 7);
    }

    #[test]
    fn test_struct_def_skipped() {
        let defs = vec![
            make_module("test"),
            Def::DStruct {
                loc: loc(),
                name: "Point".to_string(),
                type_params: vec![],
                fields: Vec::new(),
            },
        ];
        let warns = get_warnings(&defs);
        assert!(
            warns.is_empty(),
            "Struct defs should not generate warnings: {:?}",
            warns
        );
    }

    // ------------------------------------------------------------------
    // W0003 – DCIntro annotation type checking (anoncheck.ml port)
    // ------------------------------------------------------------------

    fn make_dcintro(content: &str) -> Def {
        Def::DCIntro {
            loc: loc(),
            content: content.to_string(),
        }
    }

    fn make_safe_func(name: &str, body: Expr) -> Def {
        make_func(name, body, Safety::Safe)
    }

    fn make_unsafe_func(name: &str, body: Expr) -> Def {
        make_func(name, body, Safety::Unsafe)
    }

    fn make_trusted_func(name: &str, body: Expr) -> Def {
        make_func(name, body, Safety::Trusted)
    }

    fn make_struct_def(name: &str) -> Def {
        Def::DStruct {
            loc: loc(),
            name: name.to_string(),
            type_params: vec![],
            fields: Vec::new(),
        }
    }

    fn make_test_def(name: &str) -> Def {
        Def::DTest {
            loc: loc(),
            name: name.to_string(),
            body: Box::new(Expr::EVoid { loc: loc() }),
        }
    }

    fn make_cfunc_unsafe(name: &str) -> Def {
        Def::DCFuncUnsafe {
            loc: loc(),
            name: name.to_string(),
            params: Vec::new(),
            returns: None,
            code: String::new(),
            safety: Safety::Unsafe,
            used_c_keyword: false,
        }
    }

    fn make_export_def(symbol: &str) -> Def {
        Def::SExport {
            loc: loc(),
            symbol: symbol.to_string(),
        }
    }

    fn make_impl_def() -> Def {
        Def::DImpl {
            loc: loc(),
            struct_name: "Foo".to_string(),
            impls: Vec::new(),
        }
    }

    fn make_cmagical(content: &str) -> Def {
        Def::DCMagical {
            loc: loc(),
            content: content.to_string(),
        }
    }

    fn get_w3(defs: &[Def]) -> Vec<Warning> {
        get_warnings(defs)
            .into_iter()
            .filter(|w| w.code == "W0003")
            .collect()
    }

    // --- No DCIntro before def (no warning) ---

    #[test]
    fn test_dcintro_no_annotation_before_def() {
        // No DCIntro, just a function — no annotation warning
        let defs = vec![
            make_module("test"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "No DCIntro = no W0003: {:?}", w3);
    }

    // --- Safe function (DFunc) annotations ---

    #[test]
    fn test_dcintro_usage_before_safe_func_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: used in main"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(
            w3.is_empty(),
            "usage before safe func should be valid: {:?}",
            w3
        );
    }

    #[test]
    fn test_dcintro_param_before_safe_func_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("param: x is the input"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(
            w3.is_empty(),
            "param before safe func should be valid: {:?}",
            w3
        );
    }

    #[test]
    fn test_dcintro_impl_before_safe_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("impl: this is an impl"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "impl before safe func should warn: {:?}", w3);
        assert!(w3[0].message.contains("invalid intro comment type"));
    }

    #[test]
    fn test_dcintro_unsafe_before_safe_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("unsafe: raw memory op"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "unsafe before safe func should warn");
    }

    #[test]
    fn test_dcintro_trusted_before_safe_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("trusted: safe wrapper"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "trusted before safe func should warn");
    }

    // --- Unsafe function (DFunc unsafe) annotations ---

    #[test]
    fn test_dcintro_unsafe_before_unsafe_func_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("unsafe: raw memory"),
            make_unsafe_func("do_stuff", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "unsafe before unsafe func should be valid");
    }

    #[test]
    fn test_dcintro_usage_before_unsafe_func_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: used internally"),
            make_unsafe_func("do_stuff", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "usage before unsafe func should be valid");
    }

    #[test]
    fn test_dcintro_impl_before_unsafe_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("impl: something"),
            make_unsafe_func("do_stuff", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "impl before unsafe func should warn");
    }

    #[test]
    fn test_dcintro_trusted_before_unsafe_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("trusted: safe wrapper"),
            make_unsafe_func("do_stuff", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "trusted before unsafe func should warn");
    }

    // --- Trusted function (DFunc trusted) annotations ---

    #[test]
    fn test_dcintro_trusted_before_trusted_func_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("trusted: verified safe"),
            make_trusted_func("trusted_op", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "trusted before trusted func should be valid");
    }

    #[test]
    fn test_dcintro_impl_before_trusted_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("impl: trait impl"),
            make_trusted_func("trusted_op", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "impl before trusted func should warn");
    }

    #[test]
    fn test_dcintro_unsafe_before_trusted_func_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("unsafe: raw"),
            make_trusted_func("trusted_op", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "unsafe before trusted func should warn");
    }

    // --- DCFuncUnsafe annotations ---

    #[test]
    fn test_dcintro_unsafe_before_cfunc_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("unsafe: C binding"),
            make_cfunc_unsafe("c_puts"),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "unsafe before cfunc should be valid");
    }

    #[test]
    fn test_dcintro_usage_before_cfunc_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: ffi call"),
            make_cfunc_unsafe("c_puts"),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "usage before cfunc should be valid");
    }

    #[test]
    fn test_dcintro_impl_before_cfunc_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("impl: something"),
            make_cfunc_unsafe("c_puts"),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "impl before cfunc should warn");
    }

    // --- DTest annotations ---

    #[test]
    fn test_dcintro_usage_before_test_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: test case"),
            make_test_def("test_foo"),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "usage before test should be valid");
    }

    #[test]
    fn test_dcintro_impl_before_test_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("impl: trait"),
            make_test_def("test_foo"),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "impl before test should warn");
    }

    // --- DStruct annotations ---

    #[test]
    fn test_dcintro_usage_before_struct_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: data type"),
            make_struct_def("Point"),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "usage before struct should be valid");
    }

    #[test]
    fn test_dcintro_impl_before_struct_valid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("impl: trait impl for struct"),
            make_struct_def("Point"),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "impl before struct should be valid");
    }

    #[test]
    fn test_dcintro_unsafe_before_struct_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("unsafe: raw"),
            make_struct_def("Point"),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "unsafe before struct should warn");
    }

    // --- Module, import, export, DImpl always warn ---

    #[test]
    fn test_dcintro_before_module_always_warns() {
        let defs = vec![make_dcintro("usage: module info"), make_module("test")];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "DCIntro before module should warn");
    }

    #[test]
    fn test_dcintro_before_import_always_warns() {
        let defs = vec![
            make_dcintro("usage: import"),
            Def::SImport {
                loc: loc(),
                path: "std/io".to_string(),
            },
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "DCIntro before import should warn");
    }

    #[test]
    fn test_dcintro_before_export_always_warns() {
        let defs = vec![make_dcintro("usage: export"), make_export_def("foo")];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "DCIntro before export should warn");
    }

    #[test]
    fn test_dcintro_before_impl_always_warns() {
        let defs = vec![make_dcintro("usage: impl"), make_impl_def()];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "DCIntro before impl should warn");
    }

    // --- DCMagical and DCIntro don't need annotations ---

    #[test]
    fn test_dcintro_before_cmagical_no_warning() {
        let defs = vec![
            make_dcintro("usage: magical"),
            make_cmagical("warning_off W0002"),
        ];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "DCIntro before DCMagical should not warn");
    }

    #[test]
    fn test_dcintro_before_dcintro_no_warning() {
        let defs = vec![make_dcintro("usage: first"), make_dcintro("usage: second")];
        let w3 = get_w3(&defs);
        assert!(w3.is_empty(), "DCIntro before DCIntro should not warn");
    }

    // --- Unknown annotation types ---

    #[test]
    fn test_dcintro_unknown_type_always_warns() {
        let defs = vec![
            make_module("test"),
            make_dcintro("foobar: unknown annotation"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "unknown annotation type should warn");
    }

    #[test]
    fn test_dcintro_no_colon_always_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("justtext"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "text without colon should be invalid");
    }

    #[test]
    fn test_dcintro_whitespace_trimmed() {
        let defs = vec![
            make_module("test"),
            make_dcintro("  usage  : some comment"),
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(
            w3.is_empty(),
            "whitespace-padded 'usage' should still be valid"
        );
    }

    // --- Multiple uses ---

    #[test]
    fn test_dcintro_multiple_annotations_all_valid() {
        // usage for test, usage for struct, param for func — all valid
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: test case"),
            make_test_def("test_foo"),
            make_dcintro("usage: data"),
            make_struct_def("Point"),
            make_dcintro("param: x coord"),
            make_safe_func("get_x", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert!(
            w3.is_empty(),
            "all valid annotations should not warn: {:?}",
            w3
        );
    }

    #[test]
    fn test_dcintro_multiple_annotations_one_invalid() {
        let defs = vec![
            make_module("test"),
            make_dcintro("usage: test case"),
            make_test_def("test_foo"),
            make_dcintro("unsafe: raw op"), // invalid for safe func
            make_safe_func("main", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(
            w3.len(),
            1,
            "one invalid annotation should produce one warning"
        );
    }

    #[test]
    fn test_dcintro_invalid_before_struct_with_more_defs_after() {
        // unsafe is invalid before struct, but param is valid before safe func
        let defs = vec![
            make_module("test"),
            make_dcintro("unsafe: raw"), // invalid for struct
            make_struct_def("Point"),
            make_dcintro("param: input"), // valid for safe func
            make_safe_func("process", Expr::EVoid { loc: loc() }),
        ];
        let w3 = get_w3(&defs);
        assert_eq!(w3.len(), 1, "only struct annotation should warn");
    }
}
