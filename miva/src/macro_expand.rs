use crate::ast::*;
use anyhow::{bail, Result};

/// Expand all macros (`include_str!`, `prints!`, `printlns!`, `assert!`)
/// in the given definition list.
///
/// This is a pure AST → AST transformation that runs after JSON parsing
/// and before semantic analysis. It recursively traverses all expressions,
/// replacing `Expr::EMacro` nodes with their expanded forms.
pub fn expand_macros(defs: &[Def]) -> Result<Vec<Def>> {
    let mut addf: Vec<Def> = Vec::new();

    let res: Vec<Def> = defs
        .iter()
        .map(|d| expand_def(d, &mut addf))
        .collect::<Result<Vec<_>>>()?;

    // Inject accumulated imports after the first definition
    // (mirrors OCaml: `first :: (!addf @ rest)`)
    Ok(if res.is_empty() {
        addf
    } else {
        let mut result = vec![res[0].clone()];
        result.extend(addf);
        result.extend(res[1..].iter().cloned());
        result
    })
}

// ---------------------------------------------------------------------------
// Internal: definition-level recursion
// ---------------------------------------------------------------------------

fn expand_def(def: &Def, addf: &mut Vec<Def>) -> Result<Def> {
    match def {
        Def::DFunc {
            loc,
            name,
            params,
            returns,
            body,
            safety,
        } => Ok(Def::DFunc {
            loc: loc.clone(),
            name: name.clone(),
            params: params.clone(),
            returns: returns.clone(),
            body: Box::new(expand_expr(body, addf)?),
            safety: safety.clone(),
        }),
        Def::DTest { loc, name, body } => Ok(Def::DTest {
            loc: loc.clone(),
            name: name.clone(),
            body: Box::new(expand_expr(body, addf)?),
        }),
        // DStruct, SImport, SExport, DModule, etc. pass through unchanged
        _ => Ok(def.clone()),
    }
}

// ---------------------------------------------------------------------------
// Internal: statement-level recursion
// ---------------------------------------------------------------------------

fn expand_stmt(stmt: &Stmt, addf: &mut Vec<Def>) -> Result<Stmt> {
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
            expr: Box::new(expand_expr(expr, addf)?),
        }),
        Stmt::SAssign { loc, name, expr } => Ok(Stmt::SAssign {
            loc: loc.clone(),
            name: name.clone(),
            expr: Box::new(expand_expr(expr, addf)?),
        }),
        Stmt::SReturn { loc, expr } => Ok(Stmt::SReturn {
            loc: loc.clone(),
            expr: Box::new(expand_expr(expr, addf)?),
        }),
        Stmt::SExpr { loc, expr } => Ok(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(expand_expr(expr, addf)?),
        }),
        // SCIntro and SEmpty carry no expressions
        Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => Ok(stmt.clone()),
    }
}

// ---------------------------------------------------------------------------
// Internal: expression-level recursion
// ---------------------------------------------------------------------------

fn expand_expr(expr: &Expr, addf: &mut Vec<Def>) -> Result<Expr> {
    match expr {
        // ── Macro call ──────────────────────────────────────────────────
        Expr::EMacro { loc, name, args } => expand_macro(loc, name, args, addf),

        // ── Recursive (has sub-expressions) ─────────────────────────────
        Expr::EStructLit { loc, name, fields } => {
            let expanded = fields
                .iter()
                .map(|f| {
                    expand_expr(&f.value, addf)
                        .map(|v| ValueField { name: f.name.clone(), value: v })
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EStructLit {
                loc: loc.clone(),
                name: name.clone(),
                fields: expanded,
            })
        }
        Expr::EFieldAccess { loc, expr: e, field } => Ok(Expr::EFieldAccess {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf)?),
            field: field.clone(),
        }),
        Expr::EBinOp { loc, op, left, right } => Ok(Expr::EBinOp {
            loc: loc.clone(),
            op: op.clone(),
            left: Box::new(expand_expr(left, addf)?),
            right: Box::new(expand_expr(right, addf)?),
        }),
        Expr::EIf {
            loc,
            cond,
            then,
            else_,
        } => Ok(Expr::EIf {
            loc: loc.clone(),
            cond: Box::new(expand_expr(cond, addf)?),
            then: Box::new(expand_expr(then, addf)?),
            else_: else_
                .as_ref()
                .map(|e| expand_expr(e, addf).map(Box::new))
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
                        when: Box::new(expand_expr(&c.when, addf)?),
                        then: Box::new(expand_expr(&c.then, addf)?),
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EChoose {
                loc: loc.clone(),
                var: Box::new(expand_expr(var, addf)?),
                cases,
                otherwise: otherwise
                    .as_ref()
                    .map(|e| expand_expr(e, addf).map(Box::new))
                    .transpose()?,
            })
        }
        Expr::ECall { loc, name, args } => {
            let args = args
                .iter()
                .map(|a| expand_expr(a, addf))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::ECall {
                loc: loc.clone(),
                name: name.clone(),
                args,
            })
        }
        Expr::ECast { loc, expr: e, to } => Ok(Expr::ECast {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf)?),
            to: to.clone(),
        }),
        Expr::EBlock { loc, stmts, result } => {
            let stmts = stmts
                .iter()
                .map(|s| expand_stmt(s, addf))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EBlock {
                loc: loc.clone(),
                stmts,
                result: result
                    .as_ref()
                    .map(|e| expand_expr(e, addf).map(Box::new))
                    .transpose()?,
            })
        }
        Expr::EArrayLit { loc, values } => {
            let values = values
                .iter()
                .map(|v| expand_expr(v, addf))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expr::EArrayLit {
                loc: loc.clone(),
                values,
            })
        }
        Expr::EAddr { loc, expr: e } => Ok(Expr::EAddr {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf)?),
        }),
        Expr::EDeref { loc, expr: e } => Ok(Expr::EDeref {
            loc: loc.clone(),
            expr: Box::new(expand_expr(e, addf)?),
        }),
        Expr::EFor {
            loc,
            var,
            range,
            body,
        } => Ok(Expr::EFor {
            loc: loc.clone(),
            var: var.clone(),
            range: Box::new(expand_expr(range, addf)?),
            body: Box::new(expand_expr(body, addf)?),
        }),
        Expr::ELoop { loc, body } => Ok(Expr::ELoop {
            loc: loc.clone(),
            body: Box::new(expand_expr(body, addf)?),
        }),
        Expr::EWhile { loc, cond, body } => Ok(Expr::EWhile {
            loc: loc.clone(),
            cond: Box::new(expand_expr(cond, addf)?),
            body: Box::new(expand_expr(body, addf)?),
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

fn expand_macro(loc: &Loc, name: &str, args: &[Expr], addf: &mut Vec<Def>) -> Result<Expr> {
    match name {
        "include_str" => expand_include_str(loc, args),
        "prints" => expand_prints(loc, args, addf, false),
        "printlns" => expand_prints(loc, args, addf, true),
        "assert" => expand_assert(loc, args, addf),
        _ => bail!("Unknown macro: {}", name),
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
fn expand_prints(loc: &Loc, args: &[Expr], addf: &mut Vec<Def>, add_newline: bool) -> Result<Expr> {
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
        let expanded_arg = expand_expr(arg, addf)?;
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
fn expand_assert(loc: &Loc, args: &[Expr], addf: &mut Vec<Def>) -> Result<Expr> {
    match args {
        [e] => {
            let expanded_cond = expand_expr(e, addf)?;
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
            params: vec![],
            returns: None,
            body: Box::new(body),
            safety: Safety::Safe,
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

        let result = expand_macros(&defs)?;

        // Expect: DFunc + SImport("std/str")
        assert_eq!(result.len(), 2, "should inject std/str import");

        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, result: None, .. } => {
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

        let result = expand_macros(&defs)?;
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

        let result = expand_macros(&defs)?;
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
                                    Expr::EBlock { stmts: then_stmts, .. } => {
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

        let result = expand_macros(&defs)?;
        assert_eq!(result.len(), 1);

        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EString { value, .. } => {
                    // The value should be the escaped file content.
                    // "hello\nworld" after escape_default → "hello\\nworld"
                    assert_eq!(
                        value,
                        "hello\\nworld",
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

        let result = expand_macros(&defs);
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

        let result = expand_macros(&defs)?;
        assert_eq!(result.len(), 2, "prints! injects std/str import");

        // The body should be a block (prints! expansion) where the assign
        // contains a call to string_from(…) whose argument is itself a block
        // (the assert! expansion).  We just check it doesn't error and the
        // structure is roughly right.
        match &result[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => {
                    assert!(!stmts.is_empty(), "nested macro expansion should produce stmts");
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
        let result = expand_macros(&defs)?;
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
                                Expr::EBlock { stmts: inner_stmts, .. } => {
                                    assert_eq!(inner_stmts.len(), 1);
                                    match &inner_stmts[0] {
                                        Stmt::SExpr { expr: inner_expr, .. } => {
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
        let result = expand_macros(&[])?;
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
            },
            Def::DFunc {
                loc: loc(2, 1),
                name: "foo".to_string(),
                params: vec![],
                returns: None,
                body: Box::new(Expr::EInt {
                    loc: loc(2, 6),
                    value: 0,
                }),
                safety: Safety::Safe,
            },
        ];

        let result = expand_macros(&defs)?;
        assert_eq!(result.len(), 2, "non-macro defs pass through");
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

        let result = expand_macros(&defs);
        assert!(result.is_err(), "include_str! on missing file should error");
    }
}
