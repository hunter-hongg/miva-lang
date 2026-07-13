use std::collections::HashMap;

use crate::ast::*;
use anyhow::{bail, Result};

/// A macro definition collected from source.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct MacroDef {
    pub name: String,
    pub params: Vec<MacroParam>,
    pub body: Box<Expr>,
}

/// Global macro table: short macro name → definition.
pub type MacroTable = HashMap<String, MacroDef>;

/// Collect `DMacro` definitions from a raw (pre-expansion) def list.
pub fn collect_macros(defs: &[Def]) -> MacroTable {
    let mut table = MacroTable::new();
    for def in defs {
        if let Def::DMacro {
            name, params, body, ..
        } = def
        {
            table.insert(
                name.clone(),
                MacroDef {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.clone(),
                },
            );
        }
    }
    table
}

/// Expand all macros (`include_str!`, `prints!`, `printlns!`, `assert!`,
/// and user-defined custom macros) in the given definition list.
///
/// `macro_table` contains custom macro definitions collected from all files
/// in the project. Built-in macros are always available.
///
/// This is a pure AST → AST transformation that runs after JSON parsing
/// and before semantic analysis. It recursively traverses all expressions,
/// replacing `Expr::EMacro` nodes with their expanded forms.
/// `DMacro` definitions are removed from the output; `EMacroVar` nodes are
/// only valid inside macro bodies and will be substituted.
pub fn expand_macros(defs: &[Def], macro_table: &MacroTable) -> Result<Vec<Def>> {
    let mut addf: Vec<Def> = Vec::new();

    let res: Vec<Def> = defs
        .iter()
        .map(|d| expand_def(d, &mut addf, macro_table))
        .collect::<Result<Vec<_>>>()?;

    // Inject accumulated imports after the first definition
    // (mirrors OCaml: `first :: (!addf @ rest)`)
    let expanded = if res.is_empty() {
        addf
    } else {
        let mut result = vec![res[0].clone()];
        result.extend(addf);
        result.extend(res[1..].iter().cloned());
        result
    };

    // Filter out DMacro definitions (they've been collected and expanded)
    Ok(expanded
        .into_iter()
        .filter(|d| !matches!(d, Def::DMacro { .. }))
        .collect())
}

// ---------------------------------------------------------------------------
// Internal: definition-level recursion
// ---------------------------------------------------------------------------

fn expand_def(def: &Def, addf: &mut Vec<Def>, macro_table: &MacroTable) -> Result<Def> {
    match def {
        Def::DFunc {
            loc,
            name,
            type_params,
            params,
            returns,
            body,
            safety,
            is_async,
        } => Ok(Def::DFunc {
            loc: loc.clone(),
            name: name.clone(),
            type_params: type_params.clone(),
            params: params.clone(),
            returns: returns.clone(),
            body: Box::new(expand_expr(body, addf, macro_table)?),
            safety: safety.clone(),
            is_async: *is_async,
        }),
        Def::DTest { loc, name, body } => Ok(Def::DTest {
            loc: loc.clone(),
            name: name.clone(),
            body: Box::new(expand_expr(body, addf, macro_table)?),
        }),
        // DStruct, SImport, SExport, DModule, DMacro, etc. pass through unchanged
        _ => Ok(def.clone()),
    }
}

// ---------------------------------------------------------------------------
// Internal: statement-level recursion
// ---------------------------------------------------------------------------

fn expand_stmt(stmt: &Stmt, addf: &mut Vec<Def>, macro_table: &MacroTable) -> Result<Stmt> {
    match stmt {
        Stmt::SLet {
            loc,
            mutable,
            name,
            expr,
        } => Ok(Stmt::SLet {
            loc: loc.clone(),
            mutable: *mutable,
            name: name.clone(),
            expr: Box::new(expand_expr(expr, addf, macro_table)?),
        }),
        Stmt::SLetTyped {
            loc,
            name,
            typ,
            expr,
        } => Ok(Stmt::SLetTyped {
            loc: loc.clone(),
            name: name.clone(),
            typ: typ.clone(),
            expr: Box::new(expand_expr(expr, addf, macro_table)?),
        }),
        Stmt::SAssign { loc, name, expr } => Ok(Stmt::SAssign {
            loc: loc.clone(),
            name: name.clone(),
            expr: Box::new(expand_expr(expr, addf, macro_table)?),
        }),
        Stmt::SReturn { loc, expr } => Ok(Stmt::SReturn {
            loc: loc.clone(),
            expr: Box::new(expand_expr(expr, addf, macro_table)?),
        }),
        Stmt::SExpr { loc, expr } => Ok(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(expand_expr(expr, addf, macro_table)?),
        }),
        // SCIntro and SEmpty carry no expressions
        Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => Ok(stmt.clone()),
    }
}

// ---------------------------------------------------------------------------
// Internal: expression-level recursion
// ---------------------------------------------------------------------------

fn expand_expr(expr: &Expr, addf: &mut Vec<Def>, macro_table: &MacroTable) -> Result<Expr> {
    match expr {
        // ── Macro call ──────────────────────────────────────────────────
        Expr::EMacro { loc, name, args } => expand_macro(loc, name, args, addf, macro_table),

        // ── Recursive (has sub-expressions) ─────────────────────────────
        Expr::EMacroVar { .. } => {
            bail!("macro variable outside macro body: EMacroVar should have been substituted");
        }
        Expr::EStructLit {
            loc,
            name,
            fields,
            type_args,
        } => {
            let expanded = fields
                .iter()
                .map(|f| {
                    expand_expr(&f.value, addf, macro_table).map(|v| ValueField {
                        name: f.name.clone(),
                        value: v,
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EStructLit {
                loc: loc.clone(),
                name: name.clone(),
                fields: expanded,
                type_args: type_args.clone(),
            })
        }
        Expr::EFieldAccess {
            loc,
            expr: e,
            field,
        } => Ok(Expr::EFieldAccess {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf, macro_table)?),
            field: field.clone(),
        }),
        Expr::EMethodCall {
            loc,
            expr: e,
            method,
            type_args,
            args,
        } => {
            let expanded_receiver = expand_expr(e, addf, macro_table)?;
            let expanded_args = args
                .iter()
                .map(|a| expand_expr(a, addf, macro_table))
                .collect::<Result<Vec<_>>>()?;
            let mut new_args = vec![expanded_receiver];
            new_args.extend(expanded_args);
            Ok(Expr::ECall {
                loc: loc.clone(),
                name: method.clone(),
                type_args: type_args.clone(),
                args: new_args,
            })
        }
        Expr::EBinOp {
            loc,
            op,
            left,
            right,
        } => Ok(Expr::EBinOp {
            loc: loc.clone(),
            op: op.clone(),
            left: Box::new(expand_expr(left, addf, macro_table)?),
            right: Box::new(expand_expr(right, addf, macro_table)?),
        }),
        Expr::EIf {
            loc,
            cond,
            then,
            else_,
        } => Ok(Expr::EIf {
            loc: loc.clone(),
            cond: Box::new(expand_expr(cond, addf, macro_table)?),
            then: Box::new(expand_expr(then, addf, macro_table)?),
            else_: else_
                .as_ref()
                .map(|e| expand_expr(e, addf, macro_table).map(Box::new))
                .transpose()?,
        }),
        Expr::EChoose {
            loc,
            var,
            cases,
            otherwise,
        } => {
            let cases = cases
                .iter()
                .map(|c| {
                    Ok(WhenCase {
                        when: Box::new(expand_expr(&c.when, addf, macro_table)?),
                        then: Box::new(expand_expr(&c.then, addf, macro_table)?),
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EChoose {
                loc: loc.clone(),
                var: Box::new(expand_expr(var, addf, macro_table)?),
                cases,
                otherwise: otherwise
                    .as_ref()
                    .map(|e| expand_expr(e, addf, macro_table).map(Box::new))
                    .transpose()?,
            })
        }
        Expr::ECall {
            loc,
            name,
            type_args,
            args,
        } => {
            let args = args
                .iter()
                .map(|a| expand_expr(a, addf, macro_table))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::ECall {
                loc: loc.clone(),
                name: name.clone(),
                type_args: type_args.clone(),
                args,
            })
        }
        Expr::ECast { loc, expr: e, to } => Ok(Expr::ECast {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf, macro_table)?),
            to: to.clone(),
        }),
        Expr::EBlock { loc, stmts, result } => {
            let stmts = stmts
                .iter()
                .map(|s| expand_stmt(s, addf, macro_table))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EBlock {
                loc: loc.clone(),
                stmts,
                result: result
                    .as_ref()
                    .map(|e| expand_expr(e, addf, macro_table).map(Box::new))
                    .transpose()?,
            })
        }
        Expr::EArrayLit { loc, values } => {
            let values = values
                .iter()
                .map(|v| expand_expr(v, addf, macro_table))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EArrayLit {
                loc: loc.clone(),
                values,
            })
        }
        Expr::EAddr { loc, expr: e } => Ok(Expr::EAddr {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf, macro_table)?),
        }),
        Expr::EDeref { loc, expr: e } => Ok(Expr::EDeref {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf, macro_table)?),
        }),
        Expr::EFor {
            loc,
            var,
            range,
            body,
        } => Ok(Expr::EFor {
            loc: loc.clone(),
            var: var.clone(),
            range: Box::new(expand_expr(range, addf, macro_table)?),
            body: Box::new(expand_expr(body, addf, macro_table)?),
        }),
        Expr::ELoop { loc, body } => Ok(Expr::ELoop {
            loc: loc.clone(),
            body: Box::new(expand_expr(body, addf, macro_table)?),
        }),
        Expr::EWhile { loc, cond, body } => Ok(Expr::EWhile {
            loc: loc.clone(),
            cond: Box::new(expand_expr(cond, addf, macro_table)?),
            body: Box::new(expand_expr(body, addf, macro_table)?),
        }),

        // ── Leaf types (no sub-expressions) ─────────────────────────────
        Expr::EInt { .. }
        | Expr::EBool { .. }
        | Expr::EFloat { .. }
        | Expr::EChar { .. }
        | Expr::EString { .. }
        | Expr::EVar { .. }
        | Expr::EMove { .. }
        | Expr::EClone { .. }
        | Expr::EVoid { .. } => Ok(expr.clone()),
    }
}

// ---------------------------------------------------------------------------
// Individual macro expansions
// ---------------------------------------------------------------------------

fn expand_macro(
    loc: &Loc,
    name: &str,
    args: &[Expr],
    addf: &mut Vec<Def>,
    macro_table: &MacroTable,
) -> Result<Expr> {
    // 1. Check built-in macros first
    match name {
        "include_str" => return expand_include_str(loc, args),
        "prints" => return expand_prints(loc, args, addf, false, macro_table),
        "printlns" => return expand_prints(loc, args, addf, true, macro_table),
        "assert" => return expand_assert(loc, args, addf, macro_table),
        _ => {}
    }

    // 2. Check custom macro table
    if let Some(macro_def) = macro_table.get(name) {
        if args.len() != macro_def.params.len() {
            bail!(
                "macro '{}' takes {} arguments but {} were given",
                name,
                macro_def.params.len(),
                args.len()
            );
        }
        // Substitute EMacroVar placeholders in the body with actual arguments
        let param_names: Vec<&str> = macro_def.params.iter().map(|p| p.name.as_str()).collect();
        let substituted = substitute_macro_vars(&macro_def.body, args, &param_names);
        // Recursively expand any macros in the expanded body
        return expand_expr(&substituted, addf, macro_table);
    }

    // 3. Unknown macro
    bail!("Unknown macro: {}", name);
}

/// Walk an expression tree and replace `EMacroVar { name, .. }` nodes
/// with the corresponding argument expression from `args`.
fn substitute_macro_vars(expr: &Expr, args: &[Expr], param_names: &[&str]) -> Expr {
    match expr {
        Expr::EMacroVar { name, .. } => {
            // Find the index of this param name and return the corresponding arg
            let idx = param_names
                .iter()
                .position(|n| *n == name)
                .unwrap_or_else(|| panic!("macro variable ${} not found in parameter list", name));
            args[idx].clone()
        }
        // Recursive cases
        Expr::EStructLit {
            loc,
            name,
            fields,
            type_args,
        } => Expr::EStructLit {
            loc: loc.clone(),
            name: name.clone(),
            fields: fields
                .iter()
                .map(|f| ValueField {
                    name: f.name.clone(),
                    value: substitute_macro_vars(&f.value, args, param_names),
                })
                .collect(),
            type_args: type_args.clone(),
        },
        Expr::EFieldAccess {
            loc,
            expr: e,
            field,
        } => Expr::EFieldAccess {
            loc: loc.clone(),
            expr: Box::new(substitute_macro_vars(e, args, param_names)),
            field: field.clone(),
        },
        Expr::EMethodCall {
            loc,
            expr: e,
            method,
            type_args,
            args: method_args,
        } => {
            let expanded_receiver = substitute_macro_vars(e, args, param_names);
            let expanded_args: Vec<Expr> = method_args
                .iter()
                .map(|a| substitute_macro_vars(a, args, param_names))
                .collect();
            let mut new_args = vec![expanded_receiver];
            new_args.extend(expanded_args);
            Expr::ECall {
                loc: loc.clone(),
                name: method.clone(),
                type_args: type_args.clone(),
                args: new_args,
            }
        }
        Expr::EBinOp {
            loc,
            op,
            left,
            right,
        } => Expr::EBinOp {
            loc: loc.clone(),
            op: op.clone(),
            left: Box::new(substitute_macro_vars(left, args, param_names)),
            right: Box::new(substitute_macro_vars(right, args, param_names)),
        },
        Expr::EIf {
            loc,
            cond,
            then,
            else_,
        } => Expr::EIf {
            loc: loc.clone(),
            cond: Box::new(substitute_macro_vars(cond, args, param_names)),
            then: Box::new(substitute_macro_vars(then, args, param_names)),
            else_: else_
                .as_ref()
                .map(|e| Box::new(substitute_macro_vars(e, args, param_names))),
        },
        Expr::EChoose {
            loc,
            var,
            cases,
            otherwise,
        } => Expr::EChoose {
            loc: loc.clone(),
            var: Box::new(substitute_macro_vars(var, args, param_names)),
            cases: cases
                .iter()
                .map(|c| WhenCase {
                    when: Box::new(substitute_macro_vars(&c.when, args, param_names)),
                    then: Box::new(substitute_macro_vars(&c.then, args, param_names)),
                })
                .collect(),
            otherwise: otherwise
                .as_ref()
                .map(|e| Box::new(substitute_macro_vars(e, args, param_names))),
        },
        Expr::ECall {
            loc,
            name,
            type_args: _,
            args: cargs,
        } => Expr::ECall {
            loc: loc.clone(),
            name: name.clone(),
            type_args: vec![],
            args: cargs
                .iter()
                .map(|a| substitute_macro_vars(a, args, param_names))
                .collect(),
        },
        Expr::EMacro {
            loc,
            name,
            args: margs,
        } => Expr::EMacro {
            loc: loc.clone(),
            name: name.clone(),
            args: margs
                .iter()
                .map(|a| substitute_macro_vars(a, args, param_names))
                .collect(),
        },
        Expr::ECast { loc, expr: e, to } => Expr::ECast {
            loc: loc.clone(),
            expr: Box::new(substitute_macro_vars(e, args, param_names)),
            to: to.clone(),
        },
        Expr::EBlock { loc, stmts, result } => Expr::EBlock {
            loc: loc.clone(),
            stmts: stmts
                .iter()
                .map(|s| substitute_stmt_vars(s, args, param_names))
                .collect(),
            result: result
                .as_ref()
                .map(|e| Box::new(substitute_macro_vars(e, args, param_names))),
        },
        Expr::EArrayLit { loc, values } => Expr::EArrayLit {
            loc: loc.clone(),
            values: values
                .iter()
                .map(|v| substitute_macro_vars(v, args, param_names))
                .collect(),
        },
        Expr::EAddr { loc, expr: e } => Expr::EAddr {
            loc: loc.clone(),
            expr: Box::new(substitute_macro_vars(e, args, param_names)),
        },
        Expr::EDeref { loc, expr: e } => Expr::EDeref {
            loc: loc.clone(),
            expr: Box::new(substitute_macro_vars(e, args, param_names)),
        },
        Expr::EFor {
            loc,
            var,
            range,
            body,
        } => Expr::EFor {
            loc: loc.clone(),
            var: var.clone(),
            range: Box::new(substitute_macro_vars(range, args, param_names)),
            body: Box::new(substitute_macro_vars(body, args, param_names)),
        },
        Expr::ELoop { loc, body } => Expr::ELoop {
            loc: loc.clone(),
            body: Box::new(substitute_macro_vars(body, args, param_names)),
        },
        Expr::EWhile { loc, cond, body } => Expr::EWhile {
            loc: loc.clone(),
            cond: Box::new(substitute_macro_vars(cond, args, param_names)),
            body: Box::new(substitute_macro_vars(body, args, param_names)),
        },
        // Leaf types — no substitution needed
        Expr::EInt { .. }
        | Expr::EBool { .. }
        | Expr::EFloat { .. }
        | Expr::EChar { .. }
        | Expr::EString { .. }
        | Expr::EVar { .. }
        | Expr::EMove { .. }
        | Expr::EClone { .. }
        | Expr::EVoid { .. } => expr.clone(),
    }
}

fn substitute_stmt_vars(stmt: &Stmt, args: &[Expr], param_names: &[&str]) -> Stmt {
    match stmt {
        Stmt::SLet {
            loc,
            mutable,
            name,
            expr,
        } => Stmt::SLet {
            loc: loc.clone(),
            mutable: *mutable,
            name: name.clone(),
            expr: Box::new(substitute_macro_vars(expr, args, param_names)),
        },
        Stmt::SLetTyped {
            loc,
            name,
            typ,
            expr,
        } => Stmt::SLetTyped {
            loc: loc.clone(),
            name: name.clone(),
            typ: typ.clone(),
            expr: Box::new(substitute_macro_vars(expr, args, param_names)),
        },
        Stmt::SAssign { loc, name, expr } => Stmt::SAssign {
            loc: loc.clone(),
            name: name.clone(),
            expr: Box::new(substitute_macro_vars(expr, args, param_names)),
        },
        Stmt::SReturn { loc, expr } => Stmt::SReturn {
            loc: loc.clone(),
            expr: Box::new(substitute_macro_vars(expr, args, param_names)),
        },
        Stmt::SExpr { loc, expr } => Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(substitute_macro_vars(expr, args, param_names)),
        },
        Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => stmt.clone(),
    }
}

/// `include_str!("path")` → file contents as an escaped string literal.
fn expand_include_str(loc: &Loc, args: &[Expr]) -> Result<Expr> {
    match args {
        [Expr::EString { value: file, .. }] => {
            let content = std::fs::read_to_string(file)
                .map_err(|e| anyhow::anyhow!("include_str! failed: {}", e))?;
            let escaped = content.escape_default().collect::<String>();
            Ok(Expr::EString {
                loc: loc.clone(),
                value: escaped,
            })
        }
        _ => bail!("include_str! macro expects a single string argument"),
    }
}

/// `prints!(a, b, …)` or `printlns!(a, b, …)`.
///
/// Expands to:
/// ```text
/// let s = ""
/// s = s + string_from(a) + " "
/// s = s + string_from(b) + " "
/// print(s)
/// ```
/// `printlns!` adds an extra `print("\n")` after the block.
fn expand_prints(
    loc: &Loc,
    args: &[Expr],
    addf: &mut Vec<Def>,
    add_newline: bool,
    macro_table: &MacroTable,
) -> Result<Expr> {
    // Side-effect: inject `import std/str` at file top
    addf.push(Def::SImport {
        loc: loc.clone(),
        path: "std/str".to_string(),
    });

    let mut stmts: Vec<Stmt> = Vec::new();

    // let s = ""
    stmts.push(Stmt::SLet {
        loc: loc.clone(),
        mutable: true,
        name: "s".to_string(),
        expr: Box::new(Expr::EString {
            loc: loc.clone(),
            value: String::new(),
        }),
    });

    // For each arg: s = s + string_from(arg) + " "
    for arg in args {
        let expanded_arg = expand_expr(arg, addf, macro_table)?;
        stmts.push(Stmt::SAssign {
            loc: loc.clone(),
            name: "s".to_string(),
            expr: Box::new(Expr::EBinOp {
                loc: loc.clone(),
                op: BinOp::Add,
                left: Box::new(Expr::EBinOp {
                    loc: loc.clone(),
                    op: BinOp::Add,
                    left: Box::new(Expr::EMove {
                        loc: loc.clone(),
                        name: "s".to_string(),
                    }),
                    right: Box::new(Expr::ECall {
                        loc: loc.clone(),
                        name: "string_from".to_string(),
                        type_args: vec![],
                        args: vec![expanded_arg],
                    }),
                }),
                right: Box::new(Expr::EString {
                    loc: loc.clone(),
                    value: " ".to_string(),
                }),
            }),
        });
    }

    // print(s)
    stmts.push(Stmt::SExpr {
        loc: loc.clone(),
        expr: Box::new(Expr::ECall {
            loc: loc.clone(),
            name: "print".to_string(),
            type_args: vec![],
            args: vec![Expr::EVar {
                loc: loc.clone(),
                name: "s".to_string(),
            }],
        }),
    });

    // Optionally: print("\n")  (for printlns!)
    if add_newline {
        stmts.push(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(Expr::ECall {
                loc: loc.clone(),
                name: "print".to_string(),
                type_args: vec![],
                args: vec![Expr::EString {
                    loc: loc.clone(),
                    value: "\\n".to_string(),
                }],
            }),
        });
    }

    Ok(Expr::EBlock {
        loc: loc.clone(),
        stmts,
        result: None,
    })
}

/// `assert!(expr)` expands to:
/// ```text
/// if (expr == false) { panic("Assertion failed") }
/// ```
fn expand_assert(
    loc: &Loc,
    args: &[Expr],
    addf: &mut Vec<Def>,
    macro_table: &MacroTable,
) -> Result<Expr> {
    match args {
        [e] => {
            let expanded_cond = expand_expr(e, addf, macro_table)?;
            Ok(Expr::EBlock {
                loc: loc.clone(),
                stmts: vec![Stmt::SExpr {
                    loc: loc.clone(),
                    expr: Box::new(Expr::EIf {
                        loc: loc.clone(),
                        cond: Box::new(Expr::EBinOp {
                            loc: loc.clone(),
                            op: BinOp::Eq,
                            left: Box::new(expanded_cond),
                            right: Box::new(Expr::EBool {
                                loc: loc.clone(),
                                value: false,
                            }),
                        }),
                        then: Box::new(Expr::EBlock {
                            loc: loc.clone(),
                            stmts: vec![Stmt::SExpr {
                                loc: loc.clone(),
                                expr: Box::new(Expr::ECall {
                                    loc: loc.clone(),
                                    name: "panic".to_string(),
                                    type_args: vec![],
                                    args: vec![Expr::EString {
                                        loc: loc.clone(),
                                        value: "Assertion failed".to_string(),
                                    }],
                                }),
                            }],
                            result: None,
                        }),
                        else_: None,
                    }),
                }],
                result: None,
            })
        }
        _ => bail!("assert! macro expects a single expression argument"),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // Helper: build a minimal location
    fn loc(line: i64, col: i64) -> Loc {
        Loc { line, col }
    }

    // Helper: wrap defs in a DFunc named "main" with safety Safe
    fn wrap_main(body: Expr) -> Vec<Def> {
        vec![Def::DFunc {
            loc: loc(1, 1),
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            returns: None,
            body: Box::new(body),
            safety: Safety::Safe,
            is_async: false,
        }]
    }

    // -----------------------------------------------------------------------
    // prints!
    // -----------------------------------------------------------------------
    #[test]
    fn test_expand_prints_macro() -> Result<()> {
        let defs = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "prints".to_string(),
            args: vec![Expr::EInt {
                loc: loc(2, 9),
                value: 42,
            }],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;

        // Expect: DFunc + SImport("std/str")
        assert_eq!(result.len(), 2, "should inject std/str import");

        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock {
                    stmts,
                    result: None,
                    ..
                } => {
                    assert_eq!(stmts.len(), 3, "block: let, assign, print");
                    // First stmt: let s = ""
                    assert!(
                        matches!(&stmts[0], Stmt::SLet { name, .. } if name == "s"),
                        "first stmt should be let s = \"\""
                    );
                    // Last stmt: print(s)
                    assert!(
                        matches!(&stmts[2], Stmt::SExpr { expr, .. } if matches!(expr.as_ref(), Expr::ECall { name, .. } if name == "print")),
                        "last stmt should be print(s)"
                    );
                }
                _ => panic!("body should be EBlock"),
            },
            _ => panic!("first def should be DFunc"),
        }

        // Second def should be the injected import
        match &result[1] {
            Def::SImport { path, .. } => {
                assert_eq!(path, "std/str");
            }
            _ => panic!("second def should be SImport"),
        }

        Ok(())
    }

    #[test]
    fn test_expand_printlns_macro() -> Result<()> {
        let defs = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "printlns".to_string(),
            args: vec![Expr::EInt {
                loc: loc(2, 11),
                value: 7,
            }],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;
        assert_eq!(result.len(), 2);

        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => {
                    // printlns has 4 stmts: let, assign, print(s), print("\n")
                    assert_eq!(stmts.len(), 4, "printlns should emit 4 stmts");
                    // Last stmt is print("\n")
                    assert!(
                        matches!(&stmts[3], Stmt::SExpr { expr, .. } if matches!(expr.as_ref(), Expr::ECall { name, args, .. } if name == "print" && matches!(&args[0], Expr::EString { value, .. } if value == "\\n"))),
                        "last stmt should be print(\"\\\\n\")"
                    );
                }
                _ => panic!("body should be EBlock"),
            },
            _ => panic!("first def should be DFunc"),
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // assert!
    // -----------------------------------------------------------------------
    #[test]
    fn test_expand_assert_macro() -> Result<()> {
        let defs = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "assert".to_string(),
            args: vec![Expr::EBool {
                loc: loc(2, 9),
                value: true,
            }],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;
        assert_eq!(result.len(), 1, "assert! injects no imports");

        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => {
                    assert_eq!(stmts.len(), 1, "block should contain one SExpr");
                    if let Stmt::SExpr { expr, .. } = &stmts[0] {
                        match expr.as_ref() {
                            Expr::EIf {
                                cond,
                                then,
                                else_: None,
                                ..
                            } => {
                                // cond: expanded_arg == false
                                assert!(
                                    matches!(cond.as_ref(), Expr::EBinOp { op: BinOp::Eq, .. }),
                                    "assert condition should be Eq"
                                );
                                // then: block with panic call
                                match then.as_ref() {
                                    Expr::EBlock {
                                        stmts: then_stmts, ..
                                    } => {
                                        assert_eq!(then_stmts.len(), 1);
                                    }
                                    _ => panic!("then should be EBlock"),
                                }
                            }
                            _ => panic!("outer expr should be EIf"),
                        }
                    } else {
                        panic!("stmt should be SExpr");
                    }
                }
                _ => panic!("body should be EBlock"),
            },
            _ => panic!("first def should be DFunc"),
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // include_str!
    // -----------------------------------------------------------------------
    #[test]
    fn test_expand_include_str_macro() -> Result<()> {
        // Write a temp file
        let tmp = std::env::temp_dir().join("miva_test_include_str.txt");
        let test_content = "hello\nworld";
        std::fs::write(&tmp, test_content).expect("write temp file");

        let defs = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "include_str".to_string(),
            args: vec![Expr::EString {
                loc: loc(2, 14),
                value: tmp.to_string_lossy().to_string(),
            }],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;
        assert_eq!(result.len(), 1);

        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EString { value, .. } => {
                    // The value should be the escaped file content.
                    // "hello\nworld" after escape_default → "hello\\nworld"
                    assert_eq!(
                        value, "hello\\nworld",
                        "include_str content should be escaped"
                    );
                }
                _ => panic!("body should be EString after include_str expansion"),
            },
            _ => panic!("first def should be DFunc"),
        }

        // Cleanup
        let _ = std::fs::remove_file(&tmp);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // unknown macro
    // -----------------------------------------------------------------------
    #[test]
    fn test_unknown_macro_errors() {
        let defs = wrap_main(Expr::EMacro {
            loc: loc(1, 1),
            name: "nonexistent".to_string(),
            args: vec![],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table);
        assert!(result.is_err(), "unknown macro should return an error");
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Unknown macro"),
            "error should mention unknown macro"
        );
    }

    // -----------------------------------------------------------------------
    // nested macros
    // -----------------------------------------------------------------------
    #[test]
    fn test_nested_macros() -> Result<()> {
        // assert!(include_str!("some_file")) — but the arg to include_str!
        // isn't a string literal, so we test a simpler nesting:
        // prints!(assert!(true)) — prints! calls expand_expr on each arg
        let defs = wrap_main(Expr::EMacro {
            loc: loc(1, 1),
            name: "prints".to_string(),
            args: vec![Expr::EMacro {
                loc: loc(1, 9),
                name: "assert".to_string(),
                args: vec![Expr::EBool {
                    loc: loc(1, 17),
                    value: true,
                }],
            }],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;
        assert_eq!(result.len(), 2, "prints! injects std/str import");

        // The body should be a block (prints! expansion) where the assign
        // contains a call to string_from(…) whose argument is itself a block
        // (the assert! expansion).  We just check it doesn't error and the
        // structure is roughly right.
        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => {
                    assert!(
                        !stmts.is_empty(),
                        "nested macro expansion should produce stmts"
                    );
                }
                _ => panic!("body should be EBlock"),
            },
            _ => panic!("first def should be DFunc"),
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // macros in function body (non-top-level)
    // -----------------------------------------------------------------------
    #[test]
    fn test_macro_in_block() -> Result<()> {
        // A function that contains a block with a macro inside
        let inner_block = Expr::EBlock {
            loc: loc(2, 1),
            stmts: vec![Stmt::SExpr {
                loc: loc(3, 1),
                expr: Box::new(Expr::EMacro {
                    loc: loc(3, 1),
                    name: "assert".to_string(),
                    args: vec![Expr::EBool {
                        loc: loc(3, 9),
                        value: false,
                    }],
                }),
            }],
            result: None,
        };

        let defs = wrap_main(inner_block);
        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;
        assert_eq!(result.len(), 1);

        // The assert! inside the block should expand to EBlock([SExpr(EIf{...})]).
        // Since the macro is inside an SExpr in the outer block, after expansion
        // we get: EBlock(stmts: [SExpr(expr: EBlock(stmts: [SExpr(expr: EIf{...})]))])
        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => {
                    assert_eq!(stmts.len(), 1);
                    match &stmts[0] {
                        Stmt::SExpr { expr, .. } => {
                            // The assert! expands to an EBlock wrapping the EIf
                            match expr.as_ref() {
                                Expr::EBlock {
                                    stmts: inner_stmts, ..
                                } => {
                                    assert_eq!(inner_stmts.len(), 1);
                                    match &inner_stmts[0] {
                                        Stmt::SExpr {
                                            expr: inner_expr, ..
                                        } => {
                                            assert!(
                                                matches!(inner_expr.as_ref(), Expr::EIf { .. }),
                                                "inner expr should be EIf after assert! expansion"
                                            );
                                        }
                                        _ => panic!("inner stmt should be SExpr"),
                                    }
                                }
                                _ => panic!("assert! expands to EBlock"),
                            }
                        }
                        _ => panic!("stmt should be SExpr"),
                    }
                }
                _ => panic!("body should be EBlock"),
            },
            _ => panic!("first def should be DFunc"),
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // empty defs list
    // -----------------------------------------------------------------------
    #[test]
    fn test_empty_defs() -> Result<()> {
        let macro_table = MacroTable::new();
        let result = expand_macros(&[], &macro_table)?;
        assert!(result.is_empty(), "empty defs -> empty result");
        Ok(())
    }

    // -----------------------------------------------------------------------
    // non-macro defs pass through unchanged
    // -----------------------------------------------------------------------
    #[test]
    fn test_pass_through_defs() -> Result<()> {
        let defs = vec![
            Def::DStruct {
                loc: loc(1, 1),
                name: "Point".to_string(),
                fields: vec![FieldDef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                type_params: vec![],
            },
            Def::DFunc {
                loc: loc(2, 1),
                name: "foo".to_string(),
                type_params: vec![],
                params: vec![],
                returns: None,
                body: Box::new(Expr::EInt {
                    loc: loc(2, 6),
                    value: 0,
                }),
                safety: Safety::Safe,
                is_async: false,
            },
        ];

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table)?;
        assert_eq!(result.len(), 2, "non-macro defs pass through");
        Ok(())
    }

    // -----------------------------------------------------------------------
    // custom macro — basic substitution
    // -----------------------------------------------------------------------
    #[test]
    fn test_custom_macro_substitution() -> Result<()> {
        // Define: macro double = ($x: int) => { $x + $x };
        let macro_def = Def::DMacro {
            loc: loc(1, 1),
            name: "double".to_string(),
            params: vec![MacroParam {
                name: "x".to_string(),
                typ: Typ::TInt,
            }],
            body: Box::new(Expr::EBinOp {
                loc: loc(1, 1),
                op: BinOp::Add,
                left: Box::new(Expr::EMacroVar {
                    loc: loc(1, 1),
                    name: "x".to_string(),
                }),
                right: Box::new(Expr::EMacroVar {
                    loc: loc(1, 1),
                    name: "x".to_string(),
                }),
            }),
        };

        // Call: double!(5) inside a function
        let call = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "double".to_string(),
            args: vec![Expr::EInt {
                loc: loc(2, 10),
                value: 5,
            }],
        });

        let mut defs = vec![macro_def];
        defs.extend(call);

        let macro_table = collect_macros(&defs);
        let result = expand_macros(&defs, &macro_table)?;

        // DMacro should be filtered out; only DFunc remains
        assert_eq!(result.len(), 1, "DMacro should be filtered out");
        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBinOp {
                    op: BinOp::Add,
                    left,
                    right,
                    ..
                } => {
                    // Both left and right should be the argument (5)
                    assert!(
                        matches!(left.as_ref(), Expr::EInt { value: 5, .. }),
                        "left side should be substituted with arg 5"
                    );
                    assert!(
                        matches!(right.as_ref(), Expr::EInt { value: 5, .. }),
                        "right side should be substituted with arg 5"
                    );
                }
                _ => panic!("body should be EBinOp(Add)"),
            },
            _ => panic!("first def should be DFunc"),
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // custom macro — multiple parameters
    // -----------------------------------------------------------------------
    #[test]
    fn test_custom_macro_multiple_params() -> Result<()> {
        // Define: macro add = ($a: int, $b: int) => { $a + $b };
        let macro_def = Def::DMacro {
            loc: loc(1, 1),
            name: "add".to_string(),
            params: vec![
                MacroParam {
                    name: "a".to_string(),
                    typ: Typ::TInt,
                },
                MacroParam {
                    name: "b".to_string(),
                    typ: Typ::TInt,
                },
            ],
            body: Box::new(Expr::EBinOp {
                loc: loc(1, 1),
                op: BinOp::Add,
                left: Box::new(Expr::EMacroVar {
                    loc: loc(1, 1),
                    name: "a".to_string(),
                }),
                right: Box::new(Expr::EMacroVar {
                    loc: loc(1, 1),
                    name: "b".to_string(),
                }),
            }),
        };

        // Call: add!(3, 7)
        let call = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "add".to_string(),
            args: vec![
                Expr::EInt {
                    loc: loc(2, 7),
                    value: 3,
                },
                Expr::EInt {
                    loc: loc(2, 10),
                    value: 7,
                },
            ],
        });

        let mut defs = vec![macro_def];
        defs.extend(call);

        let macro_table = collect_macros(&defs);
        let result = expand_macros(&defs, &macro_table)?;

        assert_eq!(result.len(), 1);
        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBinOp {
                    op: BinOp::Add,
                    left,
                    right,
                    ..
                } => {
                    // First arg = 3
                    assert!(
                        matches!(left.as_ref(), Expr::EInt { value: 3, .. }),
                        "first arg should be 3, got {:?}",
                        left
                    );
                    // Second arg = 7
                    assert!(
                        matches!(right.as_ref(), Expr::EInt { value: 7, .. }),
                        "second arg should be 7, got {:?}",
                        right
                    );
                }
                _ => panic!("body should be EBinOp(Add)"),
            },
            _ => panic!("first def should be DFunc"),
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // custom macro — wrong argument count
    // -----------------------------------------------------------------------
    #[test]
    fn test_custom_macro_wrong_arg_count() {
        let macro_def = Def::DMacro {
            loc: loc(1, 1),
            name: "f".to_string(),
            params: vec![MacroParam {
                name: "x".to_string(),
                typ: Typ::TInt,
            }],
            body: Box::new(Expr::EMacroVar {
                loc: loc(1, 1),
                name: "x".to_string(),
            }),
        };
        let call = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "f".to_string(),
            args: vec![], // 0 arguments, but expected 1
        });

        let mut defs = vec![macro_def];
        defs.extend(call);

        let macro_table = collect_macros(&defs);
        let result = expand_macros(&defs, &macro_table);
        assert!(result.is_err(), "wrong arg count should error");
        let err = result.unwrap_err();
        assert!(
            err.to_string()
                .contains("takes 1 arguments but 0 were given"),
            "error should mention arg count mismatch"
        );
    }

    // -----------------------------------------------------------------------
    // custom macro — macro inside macro body (nested expansion)
    // -----------------------------------------------------------------------
    #[test]
    fn test_custom_macro_nested_builtin() -> Result<()> {
        // Define: macro show = ($v: int) => { prints!($v); };
        let body = Expr::EMacro {
            loc: loc(1, 1),
            name: "prints".to_string(),
            args: vec![Expr::EMacroVar {
                loc: loc(1, 1),
                name: "v".to_string(),
            }],
        };
        let macro_def = Def::DMacro {
            loc: loc(1, 1),
            name: "show".to_string(),
            params: vec![MacroParam {
                name: "v".to_string(),
                typ: Typ::TInt,
            }],
            body: Box::new(body),
        };

        let call = wrap_main(Expr::EMacro {
            loc: loc(2, 1),
            name: "show".to_string(),
            args: vec![Expr::EInt {
                loc: loc(2, 8),
                value: 42,
            }],
        });

        let mut defs = vec![macro_def];
        defs.extend(call);

        let macro_table = collect_macros(&defs);
        let result = expand_macros(&defs, &macro_table)?;

        // After expansion: DMacro is filtered out. Prints! injects SImport into addf,
        // which is inserted after the first non-DMacro def → [SImport, DFunc]
        assert_eq!(result.len(), 2, "custom macro + prints! expansion");
        match &result[0] {
            Def::SImport { path, .. } => assert_eq!(path, "std/str"),
            _ => panic!("first should be import from prints! expansion"),
        }
        match &result[1] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => {
                    assert!(!stmts.is_empty(), "show! should expand to a block");
                }
                _ => panic!("body should be EBlock"),
            },
            _ => panic!("second should be DFunc"),
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // include_str on non-existent file
    // -----------------------------------------------------------------------
    #[test]
    fn test_include_str_file_not_found() {
        let defs = wrap_main(Expr::EMacro {
            loc: loc(1, 1),
            name: "include_str".to_string(),
            args: vec![Expr::EString {
                loc: loc(1, 14),
                value: "/nonexistent/file/for/miva_test.txt".to_string(),
            }],
        });

        let macro_table = MacroTable::new();
        let result = expand_macros(&defs, &macro_table);
        assert!(result.is_err(), "include_str! on missing file should error");
    }
}
