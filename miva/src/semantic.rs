#![allow(dead_code)]
#![allow(unused_variables)]
use std::collections::HashMap;

use crate::ast::*;
use crate::error::Error;
use crate::symbol_table::SymbolTable;

#[derive(Debug, Clone, PartialEq)]
enum VarState {
    Valid,
    Moved,
}

#[derive(Debug, Clone)]
struct VarInfo {
    typ: Typ,
    state: VarState,
    is_mutable: bool,
    is_ref_param: bool,
}

#[derive(Debug, Clone)]
struct Context {
    types: HashMap<String, Vec<FieldDef>>,
    vars: HashMap<String, VarInfo>,
}

fn is_copy_type(types: &HashMap<String, Vec<FieldDef>>, t: &Typ) -> bool {
    match t {
        Typ::TInt | Typ::TBool | Typ::TFloat32 | Typ::TFloat64 | Typ::TChar => true,
        Typ::TString
        | Typ::TArray { .. }
        | Typ::TPtr { .. }
        | Typ::TBox { .. }
        | Typ::TNull
        | Typ::TInvalid
        | Typ::TPtrAny => false,
        Typ::TStruct { name, .. } => {
            if let Some(fields) = types.get(name) {
                fields.iter().all(|f| is_copy_type(types, &f.typ))
            } else {
                false
            }
        }
    }
}

fn check_expr(ctx: &mut Context, symbol_table: &SymbolTable, e: &Expr) -> Vec<Error> {
    let mut errs = Vec::new();

    let mut mark_moved = |ctx: &mut Context, loc: &Loc, name: &str| {
        if let Some(info) = ctx.vars.get(name) {
            if info.state == VarState::Moved {
                errs.push(Error::new(
                    "E0001",
                    loc,
                    &format!("use of moved value {}", name),
                ));
            }
            if info.is_ref_param {
                errs.push(Error::new(
                    "E0002",
                    loc,
                    &format!("cannot move ref parameter {}", name),
                ));
            }
            ctx.vars.insert(
                name.to_string(),
                VarInfo {
                    state: VarState::Moved,
                    ..info.clone()
                },
            );
        }
    };

    match e {
        Expr::EVar { loc, name } => {
            if let Some(info) = ctx.vars.get(name) {
                if info.state == VarState::Moved {
                    errs.push(Error::new(
                        "E0001",
                        loc,
                        &format!("use of moved value {}", name),
                    ));
                }
            } else {
                errs.push(Error::new(
                    "E0007",
                    loc,
                    &format!("variable '{}' not found", name),
                ));
            }
        }
        Expr::EMove { loc, name } => {
            if let Some(info) = ctx.vars.get(name) {
                mark_moved(ctx, loc, name);
            } else {
                errs.push(Error::new(
                    "E0007",
                    loc,
                    &format!("variable '{}' not found", name),
                ));
            }
        }
        Expr::EClone { loc, name } => {
            if let Some(info) = ctx.vars.get(name) {
                if info.state == VarState::Moved {
                    errs.push(Error::new(
                        "E0001",
                        loc,
                        &format!("use of moved value {}", name),
                    ));
                }
            } else {
                errs.push(Error::new(
                    "E0007",
                    loc,
                    &format!("variable '{}' not found", name),
                ));
            }
        }
        Expr::ECall { loc, name, args } => {
            match symbol_table.get_function_safety(name) {
                Some(Safety::Unsafe) => {
                    errs.push(Error::new(
                        "E0009",
                        loc,
                        &format!("cannot call unsafe function '{}' from safe function", name),
                    ));
                }
                Some(Safety::Trusted) | Some(Safety::Safe) => {}
                None => {
                    if name.starts_with("ffi.") {
                        errs.push(Error::new(
                            "E0009",
                            loc,
                            &format!(
                                "cannot call unsafe ffi function '{}' from safe function",
                                name
                            ),
                        ));
                    } else {
                        errs.push(Error::new(
                            "E0009",
                            loc,
                            &format!("unknown function: {}", name),
                        ));
                    }
                }
            }
            for arg in args {
                errs.extend(check_expr(ctx, symbol_table, arg));
            }
        }
        Expr::EStructLit { loc, fields, .. } => {
            for vf in fields {
                errs.extend(check_expr(ctx, symbol_table, &vf.value));
            }
        }
        Expr::EArrayLit { loc, values } => {
            for elem in values {
                errs.extend(check_expr(ctx, symbol_table, elem));
            }
        }
        Expr::EFieldAccess { loc, expr, .. } => {
            errs.extend(check_expr(ctx, symbol_table, expr));
        }
        Expr::EBinOp {
            loc, left, right, ..
        } => {
            errs.extend(check_expr(ctx, symbol_table, left));
            errs.extend(check_expr(ctx, symbol_table, right));
        }
        Expr::EIf {
            loc,
            cond,
            then,
            else_,
            ..
        } => {
            errs.extend(check_expr(ctx, symbol_table, cond));

            let vars_before = ctx.vars.clone();

            errs.extend(check_expr(ctx, symbol_table, then));
            let vars_after_then = ctx.vars.clone();

            ctx.vars = vars_before.clone();
            let vars_after_else = match else_ {
                Some(e) => {
                    errs.extend(check_expr(ctx, symbol_table, e));
                    ctx.vars.clone()
                }
                None => vars_before,
            };

            let merged_vars = merge_var_maps(&vars_after_then, &vars_after_else);
            ctx.vars = merged_vars;
        }
        Expr::EChoose {
            loc,
            var,
            cases,
            otherwise,
            ..
        } => {
            if otherwise.is_none() {
                errs.push(Error::new(
                    "E0011",
                    loc,
                    "choose expression must have an otherwise branch",
                ));
            }

            errs.extend(check_expr(ctx, symbol_table, var));

            let vars_before = ctx.vars.clone();

            let mut branch_vars = Vec::new();
            for case in cases {
                ctx.vars = vars_before.clone();
                errs.extend(check_expr(ctx, symbol_table, &case.when));
                errs.extend(check_expr(ctx, symbol_table, &case.then));
                branch_vars.push(ctx.vars.clone());
            }

            ctx.vars = vars_before.clone();
            let otherwise_vars = match otherwise {
                Some(body) => {
                    errs.extend(check_expr(ctx, symbol_table, body));
                    ctx.vars.clone()
                }
                None => vars_before.clone(),
            };

            let all_branch_vars: Vec<_> = branch_vars
                .into_iter()
                .chain(Some(otherwise_vars))
                .collect();

            let merged_vars = all_branch_vars
                .into_iter()
                .fold(vars_before.clone(), |acc, branch_vars| {
                    merge_var_maps(&acc, &branch_vars)
                });
            ctx.vars = merged_vars;
        }
        Expr::EBlock {
            loc: _,
            stmts,
            result,
            ..
        } => {
            let saved_vars = ctx.vars.clone();

            for stmt in stmts {
                match stmt {
                    Stmt::SLet {
                        loc,
                        mutable,
                        name,
                        expr,
                    } => {
                        errs.extend(check_expr(ctx, symbol_table, expr));
                        ctx.vars.insert(
                            name.clone(),
                            VarInfo {
                                typ: Typ::TInt,
                                state: VarState::Valid,
                                is_mutable: *mutable,
                                is_ref_param: false,
                            },
                        );
                    }
                    Stmt::SLetTyped { loc, name, typ, expr } => {
                        errs.extend(check_expr(ctx, symbol_table, expr));
                        ctx.vars.insert(
                            name.clone(),
                            VarInfo {
                                typ: typ.clone(),
                                state: VarState::Valid,
                                is_mutable: false,
                                is_ref_param: false,
                            },
                        );
                    }
                    Stmt::SAssign { loc, name, expr } => {
                        if let Some(var_info) = ctx.vars.get(name.as_str()) {
                            let is_mutable = var_info.is_mutable;
                            let is_moved = var_info.state == VarState::Moved;
                            let var_info_clone = var_info.clone();
                            if !is_mutable {
                                errs.push(Error::new(
                                    "E0002",
                                    loc,
                                    &format!("cannot assign to immutable variable: {}", name),
                                ));
                            }
                            if is_moved {
                                errs.push(Error::new(
                                    "E0001",
                                    loc,
                                    &format!("use of moved value {}", name),
                                ));
                            }
                            errs.extend(check_expr(ctx, symbol_table, expr));
                            ctx.vars.insert(
                                name.clone(),
                                VarInfo {
                                    state: VarState::Valid,
                                    ..var_info_clone
                                },
                            );
                        }
                    }
                    Stmt::SReturn { loc, expr } => {
                        errs.extend(check_expr(ctx, symbol_table, expr));
                    }
                    Stmt::SExpr { loc, expr } => {
                        errs.extend(check_expr(ctx, symbol_table, expr));
                    }
                    Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => {}
                }
            }

            if let Some(e) = result {
                errs.extend(check_expr(ctx, symbol_table, e));
            }

            let propagated_vars = merge_saved_with_current(&saved_vars, &ctx.vars);
            ctx.vars = propagated_vars;
        }
        Expr::ECast { loc, expr, .. } => {
            errs.extend(check_expr(ctx, symbol_table, expr));
        }
        Expr::EDeref { loc, .. } => {
            errs.push(Error::new(
                "E0010",
                loc,
                "cannot dereference a ptr in a safe function.",
            ));
        }
        Expr::EWhile { loc, cond, body } => {
            errs.extend(check_expr(ctx, symbol_table, cond));
            errs.extend(check_expr(ctx, symbol_table, body));
        }
        Expr::ELoop { loc, body } => {
            errs.extend(check_expr(ctx, symbol_table, body));
        }
        Expr::EFor {
            loc,
            var,
            range,
            body,
        } => {
            errs.extend(check_expr(ctx, symbol_table, range));
            ctx.vars.insert(
                var.clone(),
                VarInfo {
                    typ: Typ::TInt,
                    state: VarState::Valid,
                    is_mutable: false,
                    is_ref_param: false,
                },
            );
            errs.extend(check_expr(ctx, symbol_table, body));
        }
        Expr::EInt { .. }
        | Expr::EBool { .. }
        | Expr::EFloat { .. }
        | Expr::EChar { .. }
        | Expr::EString { .. }
        | Expr::EVoid { .. }
        | Expr::EMacro { .. }
        | Expr::EMacroVar { .. }
        | Expr::EAddr { .. } => {}
    }

    errs
}

fn merge_var_maps(
    a: &HashMap<String, VarInfo>,
    b: &HashMap<String, VarInfo>,
) -> HashMap<String, VarInfo> {
    let mut result = HashMap::new();
    let all_keys: std::collections::HashSet<&String> = a.keys().chain(b.keys()).collect();
    for key in all_keys {
        match (a.get(key), b.get(key)) {
            (Some(info1), Some(info2)) => {
                if info1.state == VarState::Moved || info2.state == VarState::Moved {
                    result.insert(
                        key.clone(),
                        VarInfo {
                            state: VarState::Moved,
                            ..info1.clone()
                        },
                    );
                } else {
                    result.insert(key.clone(), info1.clone());
                }
            }
            (Some(info), None) | (None, Some(info)) => {
                result.insert(key.clone(), info.clone());
            }
            (None, None) => {}
        }
    }
    result
}

fn merge_saved_with_current(
    saved: &HashMap<String, VarInfo>,
    current: &HashMap<String, VarInfo>,
) -> HashMap<String, VarInfo> {
    let mut result = HashMap::new();
    let all_keys: std::collections::HashSet<&String> = saved.keys().chain(current.keys()).collect();
    for key in all_keys {
        match (saved.get(key), current.get(key)) {
            (Some(saved_info), Some(current_info)) => {
                if current_info.state == VarState::Moved {
                    result.insert(
                        key.clone(),
                        VarInfo {
                            state: VarState::Moved,
                            ..saved_info.clone()
                        },
                    );
                } else {
                    result.insert(key.clone(), saved_info.clone());
                }
            }
            (Some(saved_info), None) => {
                result.insert(key.clone(), saved_info.clone());
            }
            (None, Some(_)) => {}
            (None, None) => {}
        }
    }
    result
}

pub fn check_program(defs: &[Def]) -> Vec<Error> {
    let (symbol_table, mut errs) = SymbolTable::build_with_errors(defs);

    let types: HashMap<String, Vec<FieldDef>> = defs
        .iter()
        .filter_map(|d| match d {
            Def::DStruct { name, fields, .. } => Some((name.clone(), fields.clone())),
            _ => None,
        })
        .collect();

    for def in defs {
        match def {
            Def::DModule { loc, name } => {
                let module_decls: Vec<_> = defs
                    .iter()
                    .filter_map(|d| match d {
                        Def::DModule { name, .. } => Some(name.clone()),
                        _ => None,
                    })
                    .collect();
                let has_non_module_before = defs
                    .iter()
                    .position(|d| matches!(d, Def::DModule { .. }))
                    .map_or(false, |pos| pos > 0);
                if has_non_module_before {
                    errs.push(Error::new(
                        "E0005",
                        loc,
                        &format!("module declaration must be at the top of the file"),
                    ));
                }
                if module_decls.iter().filter(|n| *n == name).count() > 1 {
                    errs.push(Error::new("E0005", loc, "duplicate module declaration"));
                }
                if module_decls.len() > 1 {
                    errs.push(Error::new(
                        "E0005",
                        loc,
                        "program must have only one module declaration",
                    ));
                }
            }
            Def::DFunc {
                loc: _,
                name: _,
                params,
                body,
                ..
            } => {
                let vars: HashMap<String, VarInfo> = params
                    .iter()
                    .map(|p| match p {
                        Param::PRef { name, typ } => (
                            name.clone(),
                            VarInfo {
                                typ: typ.clone(),
                                state: VarState::Valid,
                                is_mutable: false,
                                is_ref_param: true,
                            },
                        ),
                        Param::POwn { name, typ } => (
                            name.clone(),
                            VarInfo {
                                typ: typ.clone(),
                                state: VarState::Valid,
                                is_mutable: false,
                                is_ref_param: false,
                            },
                        ),
                    })
                    .collect();
                let mut ctx = Context {
                    types: types.clone(),
                    vars,
                };
                errs.extend(check_expr(&mut ctx, &symbol_table, body));
            }
            Def::DCFuncUnsafe { .. } | Def::DStruct { .. } => {}
            Def::DTest {
                loc: _,
                name: _,
                body,
            } => {
                let mut ctx = Context {
                    types: types.clone(),
                    vars: HashMap::new(),
                };
                errs.extend(check_expr(&mut ctx, &symbol_table, body));
            }
            Def::DCMagical { loc, content } => {
                let s = content.trim();
                let sd: Vec<&str> = s.split(' ').collect();
                if sd.len() < 2 {
                    errs.push(Error::new("E0013", loc, "magical comments isn't valid"));
                } else {
                    match sd[0] {
                        "warning_off" | "warning_err" | "release" | "mangle" => {}
                        _ => {
                            errs.push(Error::new(
                                "E0013",
                                loc,
                                &format!("invalid magical comment {}", sd[0]),
                            ));
                        }
                    }
                }
            }
            Def::DMacro { .. }
            | Def::SExport { .. }
            | Def::SImport { .. }
            | Def::SImportAs { .. }
            | Def::SImportHere { .. }
            | Def::DCIntro { .. }
            | Def::DImpl { .. } => {}
        }
    }

    let has_module_decl = defs.iter().any(|d| matches!(d, Def::DModule { .. }));
    if !has_module_decl {
        errs.push(Error::new(
            "E0005",
            &Loc { line: 0, col: 0 },
            "program must have one module declaration",
        ));
    }

    errs
}

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

    fn make_func(name: &str, params: Vec<Param>, body: Expr, safety: Safety) -> Def {
        Def::DFunc {
            loc: loc(),
            name: name.to_string(),
            params,
            returns: None,
            body: Box::new(body),
            safety,
        }
    }

    fn make_test_def(name: &str, body: Expr) -> Def {
        Def::DTest {
            loc: loc(),
            name: name.to_string(),
            body: Box::new(body),
        }
    }

    fn make_struct(name: &str, fields: Vec<FieldDef>) -> Def {
        Def::DStruct {
            loc: loc(),
            name: name.to_string(),
            fields,
        }
    }

    #[test]
    fn test_empty_program_has_module_error() {
        let errs = check_program(&[]);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0005"));
    }

    #[test]
    fn test_valid_program_no_errors() {
        let defs = vec![
            make_module("test"),
            make_func("main", vec![], Expr::EVoid { loc: loc() }, Safety::Safe),
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "Expected no errors, got: {:?}", errs);
    }

    #[test]
    fn test_use_of_undefined_variable() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EVar {
                    loc: loc(),
                    name: "undefined_var".to_string(),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_move_then_use() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EMove {
                                loc: loc(),
                                name: "x".to_string(),
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EVar {
                                loc: loc(),
                                name: "x".to_string(),
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0001"));
    }

    #[test]
    fn test_unknown_function_call() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "nonexistent_func".to_string(),
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0009"));
    }

    #[test]
    fn test_unsafe_function_call_from_safe() {
        let defs = vec![
            make_module("test"),
            make_func(
                "dangerous",
                vec![],
                Expr::EVoid { loc: loc() },
                Safety::Unsafe,
            ),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "dangerous".to_string(),
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0009"));
        assert!(errs.iter().any(|e| e.message.contains("unsafe function")));
    }

    #[test]
    fn test_deref_in_safe_function() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EDeref {
                    loc: loc(),
                    expr: Box::new(Expr::EVoid { loc: loc() }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0010"));
    }

    #[test]
    fn test_choose_without_otherwise() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
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
                        then: Box::new(Expr::EVoid { loc: loc() }),
                    }],
                    otherwise: None,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0011"));
    }

    #[test]
    fn test_assign_to_immutable() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SLet {
                            loc: loc(),
                            mutable: false,
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 1,
                            }),
                        },
                        Stmt::SAssign {
                            loc: loc(),
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 2,
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0002"));
    }

    #[test]
    fn test_is_copy_type_primitives() {
        let types = HashMap::new();
        assert!(is_copy_type(&types, &Typ::TInt));
        assert!(is_copy_type(&types, &Typ::TBool));
        assert!(is_copy_type(&types, &Typ::TFloat32));
        assert!(is_copy_type(&types, &Typ::TFloat64));
        assert!(is_copy_type(&types, &Typ::TChar));
    }

    #[test]
    fn test_is_copy_type_non_copy() {
        let types = HashMap::new();
        assert!(!is_copy_type(&types, &Typ::TString));
        assert!(!is_copy_type(&types, &Typ::TNull));
        assert!(!is_copy_type(&types, &Typ::TPtrAny));
    }

    #[test]
    fn test_is_copy_type_struct_all_copy() {
        let mut types = HashMap::new();
        types.insert(
            "Point".to_string(),
            vec![
                FieldDef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                },
                FieldDef {
                    name: "y".to_string(),
                    typ: Typ::TInt,
                },
            ],
        );
        assert!(is_copy_type(
            &types,
            &Typ::TStruct {
                name: "Point".to_string(),
                fields: vec![],
            }
        ));
    }

    #[test]
    fn test_is_copy_type_struct_non_copy_field() {
        let mut types = HashMap::new();
        types.insert(
            "Bad".to_string(),
            vec![FieldDef {
                name: "s".to_string(),
                typ: Typ::TString,
            }],
        );
        assert!(!is_copy_type(
            &types,
            &Typ::TStruct {
                name: "Bad".to_string(),
                fields: vec![],
            }
        ));
    }

    #[test]
    fn test_if_branch_move_merge() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    then: Box::new(Expr::EMove {
                        loc: loc(),
                        name: "x".to_string(),
                    }),
                    else_: Some(Box::new(Expr::EMove {
                        loc: loc(),
                        name: "x".to_string(),
                    })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "Both branches move x, so x should be considered moved after if"
        );
    }

    #[test]
    fn test_trusted_function_call_allowed() {
        let defs = vec![
            make_module("test"),
            make_func(
                "trusted_func",
                vec![],
                Expr::EVoid { loc: loc() },
                Safety::Trusted,
            ),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "trusted_func".to_string(),
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "Trusted function calls should be allowed from safe functions"
        );
    }

    #[test]
    fn test_safe_function_call_allowed() {
        let defs = vec![
            make_module("test"),
            make_func(
                "safe_func",
                vec![],
                Expr::EVoid { loc: loc() },
                Safety::Safe,
            ),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "safe_func".to_string(),
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "Safe function calls should be allowed from safe functions"
        );
    }

    #[test]
    fn test_clone_after_move_error() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EMove {
                                loc: loc(),
                                name: "x".to_string(),
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EClone {
                                loc: loc(),
                                name: "x".to_string(),
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0001"));
    }

    #[test]
    fn test_ref_param_move_error() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![Param::PRef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                Expr::EMove {
                    loc: loc(),
                    name: "x".to_string(),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0002"));
    }

    #[test]
    fn test_assign_after_move_error() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EMove {
                                loc: loc(),
                                name: "x".to_string(),
                            }),
                        },
                        Stmt::SAssign {
                            loc: loc(),
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 5,
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0001"));
    }

    #[test]
    fn test_mutable_assign_resets_state() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SLet {
                            loc: loc(),
                            mutable: true,
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 1,
                            }),
                        },
                        Stmt::SAssign {
                            loc: loc(),
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 2,
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EVar {
                                loc: loc(),
                                name: "x".to_string(),
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "Mutable assignment should reset state to Valid"
        );
    }

    #[test]
    fn test_nested_block_scopes() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SLet {
                            loc: loc(),
                            mutable: false,
                            name: "outer".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 1,
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EBlock {
                                loc: loc(),
                                stmts: vec![Stmt::SLet {
                                    loc: loc(),
                                    mutable: false,
                                    name: "inner".to_string(),
                                    expr: Box::new(Expr::EInt {
                                        loc: loc(),
                                        value: 2,
                                    }),
                                }],
                                result: Some(Box::new(Expr::EVoid { loc: loc() })),
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EVar {
                                loc: loc(),
                                name: "outer".to_string(),
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "Outer variable should still be accessible after nested block"
        );
    }

    #[test]
    fn test_struct_lit_field_checking() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EStructLit {
                    loc: loc(),
                    name: "Point".to_string(),
                    fields: vec![ValueField {
                        name: "x".to_string(),
                        value: Expr::EVar {
                            loc: loc(),
                            name: "undefined".to_string(),
                        },
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_array_lit_element_checking() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EArrayLit {
                    loc: loc(),
                    values: vec![
                        Expr::EInt {
                            loc: loc(),
                            value: 1,
                        },
                        Expr::EVar {
                            loc: loc(),
                            name: "undefined".to_string(),
                        },
                    ],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_binop_both_sides_checked() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Add,
                    left: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "left_undefined".to_string(),
                    }),
                    right: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "right_undefined".to_string(),
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.iter().filter(|e| e.code == "E0007").count() >= 2);
    }

    #[test]
    fn test_cast_inner_checked() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::ECast {
                    loc: loc(),
                    expr: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined".to_string(),
                    }),
                    to: Typ::TInt,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_field_access_inner_checked() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EFieldAccess {
                    loc: loc(),
                    expr: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined".to_string(),
                    }),
                    field: "x".to_string(),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_while_loop_checking() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EWhile {
                    loc: loc(),
                    cond: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined_cond".to_string(),
                    }),
                    body: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined_body".to_string(),
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.iter().filter(|e| e.code == "E0007").count() >= 2);
    }

    #[test]
    fn test_loop_body_checked() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::ELoop {
                    loc: loc(),
                    body: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined".to_string(),
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_for_loop_checking() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EFor {
                    loc: loc(),
                    var: "i".to_string(),
                    range: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined_range".to_string(),
                    }),
                    body: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "undefined_body".to_string(),
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.iter().filter(|e| e.code == "E0007").count() >= 2);
    }

    #[test]
    fn test_for_loop_var_accessible_in_body() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::EFor {
                    loc: loc(),
                    var: "i".to_string(),
                    range: Box::new(Expr::EArrayLit {
                        loc: loc(),
                        values: vec![
                            Expr::EInt {
                                loc: loc(),
                                value: 1,
                            },
                            Expr::EInt {
                                loc: loc(),
                                value: 2,
                            },
                        ],
                    }),
                    body: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "i".to_string(),
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        let var_errs: Vec<_> = errs.iter().filter(|e| e.code == "E0007").collect();
        assert!(
            var_errs.is_empty(),
            "loop variable 'i' should be accessible in body, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_multiple_module_declarations_error() {
        let defs = vec![make_module("first"), make_module("second")];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0005"));
    }

    #[test]
    fn test_duplicate_module_declaration_error() {
        let defs = vec![
            make_module("test"),
            make_func("f", vec![], Expr::EVoid { loc: loc() }, Safety::Safe),
            make_module("test"),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0005"));
    }

    #[test]
    fn test_invalid_magical_comment() {
        let defs = vec![
            make_module("test"),
            Def::DCMagical {
                loc: loc(),
                content: "invalid_directive".to_string(),
            },
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0013"));
    }

    #[test]
    fn test_valid_magical_comments_no_error() {
        for directive in &[
            "warning_off foo",
            "warning_err bar",
            "release true",
            "mangle name",
        ] {
            let defs = vec![
                make_module("test"),
                Def::DCMagical {
                    loc: loc(),
                    content: directive.to_string(),
                },
            ];
            let errs = check_program(&defs);
            assert!(
                !errs.iter().any(|e| e.code == "E0013"),
                "Directive '{}' should be valid",
                directive
            );
        }
    }

    #[test]
    fn test_magical_comment_too_short() {
        let defs = vec![
            make_module("test"),
            Def::DCMagical {
                loc: loc(),
                content: "warning_off".to_string(),
            },
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0013"));
    }

    #[test]
    fn test_test_def_checks_body() {
        let defs = vec![
            make_module("test"),
            make_test_def(
                "test_foo",
                Expr::EVar {
                    loc: loc(),
                    name: "undefined_in_test".to_string(),
                },
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0007"));
    }

    #[test]
    fn test_call_with_args_checks_each_arg() {
        let defs = vec![
            make_module("test"),
            make_func("helper", vec![], Expr::EVoid { loc: loc() }, Safety::Safe),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "helper".to_string(),
                    args: vec![
                        Expr::EVar {
                            loc: loc(),
                            name: "undefined1".to_string(),
                        },
                        Expr::EVar {
                            loc: loc(),
                            name: "undefined2".to_string(),
                        },
                    ],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.iter().filter(|e| e.code == "E0007").count() >= 2);
    }

    #[test]
    fn test_builtin_function_call_known() {
        for builtin in &[
            "print",
            "println",
            "string_concat",
            "box_new",
            "range",
            "exit",
            "abort",
            "panic",
        ] {
            let defs = vec![
                make_module("test"),
                make_func(
                    "main",
                    vec![],
                    Expr::ECall {
                        loc: loc(),
                        name: builtin.to_string(),
                        args: vec![],
                    },
                    Safety::Safe,
                ),
            ];
            let errs = check_program(&defs);
            let unknown_errs: Vec<_> = errs
                .iter()
                .filter(|e| e.code == "E0009" && e.message.contains("unknown function"))
                .collect();
            assert!(
                unknown_errs.is_empty(),
                "builtin '{}' should not be unknown, errors: {:?}",
                builtin,
                errs
            );
        }
    }

    #[test]
    fn test_builtin_ptr_set_unsafe() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "ptr_set".to_string(),
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs
            .iter()
            .any(|e| e.code == "E0009" && e.message.contains("unsafe function")));
    }

    #[test]
    fn test_user_function_overrides_builtin() {
        let defs = vec![
            make_module("test"),
            make_func("print", vec![], Expr::EVoid { loc: loc() }, Safety::Safe),
            make_func(
                "main",
                vec![],
                Expr::ECall {
                    loc: loc(),
                    name: "print".to_string(),
                    args: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        let unknown_errs: Vec<_> = errs
            .iter()
            .filter(|e| e.code == "E0009" && e.message.contains("unknown function"))
            .collect();
        assert!(
            unknown_errs.is_empty(),
            "user-defined 'print' should override builtin, errors: {:?}",
            errs
        );
    }
}
