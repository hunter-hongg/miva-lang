use crate::ast::AstFile;

pub fn from_str(input: &str) -> Result<AstFile, serde_json::Error> {
    serde_json::from_str(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_empty_ast() {
        let json = r#"{"defs":[],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        assert!(result.defs.is_empty());
        assert_eq!(result.files, vec!["test.miva"]);
    }

    #[test]
    fn test_int_expr() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"foo","params":[],"returns":null,"body":{"kind":"int","loc":{"line":1,"col":10},"value":42},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        assert_eq!(result.defs.len(), 1);
        match &result.defs[0] {
            Def::DFunc { name, body, safety, .. } => {
                assert_eq!(name, "foo");
                assert!(matches!(safety, Safety::Safe));
                match body.as_ref() {
                    Expr::EInt { value, .. } => assert_eq!(*value, 42),
                    _ => panic!("expected EInt"),
                }
            }
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_all_type_variants() {
        let json = r#"{"defs":[{"kind":"struct","loc":{"line":1,"col":1},"name":"Foo","fields":[
            {"name":"a","type":{"kind":"int"}},
            {"name":"b","type":{"kind":"bool"}},
            {"name":"c","type":{"kind":"float32"}},
            {"name":"d","type":{"kind":"float64"}},
            {"name":"e","type":{"kind":"char"}},
            {"name":"f","type":{"kind":"string"}},
            {"name":"g","type":{"kind":"array","of":{"kind":"int"}}},
            {"name":"h","type":{"kind":"ptr","to":{"kind":"int"}}},
            {"name":"i","type":{"kind":"box","of":{"kind":"int"}}},
            {"name":"j","type":{"kind":"null"}},
            {"name":"k","type":{"kind":"ptrany"}},
            {"name":"l","type":{"kind":"invalid"}},
            {"name":"m","type":{"kind":"struct","name":"Bar","fields":[{"name":"x","type":{"kind":"int"}}]}}
        ]}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        assert_eq!(result.defs.len(), 1);
    }

    #[test]
    fn test_unsafe_func() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"unsafe_foo","params":[],"returns":null,"body":{"kind":"void","loc":{"line":1,"col":20}},"safety":"unsafe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { name, safety, .. } => {
                assert_eq!(name, "unsafe_foo");
                assert!(matches!(safety, Safety::Unsafe));
            }
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_trusted_func() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"trusted_foo","params":[],"returns":null,"body":{"kind":"void","loc":{"line":1,"col":20}},"safety":"trusted"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { safety, .. } => assert!(matches!(safety, Safety::Trusted)),
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_c_func() {
        let json = r#"{"defs":[{"kind":"cFunc","loc":{"line":1,"col":1},"name":"puts","params":[{"kind":"own","name":"s","type":{"kind":"string"}}],"returns":{"kind":"int"},"code":"int x = 1;","safety":"unsafe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DCFuncUnsafe { name, code, .. } => {
                assert_eq!(name, "puts");
                assert_eq!(code, "int x = 1;");
            }
            _ => panic!("expected DCFuncUnsafe"),
        }
    }

    #[test]
    fn test_if_expr() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"test_if","params":[],"returns":null,"body":{"kind":"if","loc":{"line":1,"col":10},"cond":{"kind":"bool","loc":{"line":1,"col":15},"value":true},"then":{"kind":"int","loc":{"line":1,"col":22},"value":1},"else":{"kind":"int","loc":{"line":1,"col":28},"value":0}},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EIf { cond, then, else_, .. } => {
                    assert!(matches!(cond.as_ref(), Expr::EBool { value: true, .. }));
                    assert!(matches!(then.as_ref(), Expr::EInt { value: 1, .. }));
                    assert!(else_.is_some());
                    assert!(matches!(else_.as_ref().unwrap().as_ref(), Expr::EInt { value: 0, .. }));
                }
                _ => panic!("expected EIf"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_if_no_else() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"test_if_no_else","params":[],"returns":null,"body":{"kind":"if","loc":{"line":1,"col":10},"cond":{"kind":"bool","loc":{"line":1,"col":15},"value":true},"then":{"kind":"int","loc":{"line":1,"col":22},"value":1},"else":null},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EIf { else_, .. } => assert!(else_.is_none()),
                _ => panic!("expected EIf"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_block_expr() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"test_block","params":[],"returns":null,"body":{"kind":"block","loc":{"line":1,"col":10},"stmts":[{"kind":"let","loc":{"line":2,"col":5},"mutable":false,"name":"x","expr":{"kind":"int","loc":{"line":2,"col":10},"value":1}}],"result":null},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, result, .. } => {
                    assert_eq!(stmts.len(), 1);
                    assert!(matches!(&stmts[0], Stmt::SLet { name, .. } if name == "x"));
                    assert!(result.is_none());
                }
                _ => panic!("expected EBlock"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_binop_expr() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"add","params":[{"kind":"own","name":"a","type":{"kind":"int"}},{"kind":"own","name":"b","type":{"kind":"int"}}],"returns":{"kind":"int"},"body":{"kind":"binOp","loc":{"line":1,"col":20},"op":"add","left":{"kind":"var","loc":{"line":1,"col":15},"name":"a"},"right":{"kind":"var","loc":{"line":1,"col":18},"name":"b"}},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBinOp { op, left, right, .. } => {
                    assert!(matches!(op, BinOp::Add));
                    assert!(matches!(left.as_ref(), Expr::EVar { name, .. } if name == "a"));
                    assert!(matches!(right.as_ref(), Expr::EVar { name, .. } if name == "b"));
                }
                _ => panic!("expected EBinOp"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_module_import_export() {
        let json = r#"{"defs":[
            {"kind":"module","loc":{"line":1,"col":1},"name":"std.io"},
            {"kind":"import","loc":{"line":2,"col":1},"path":"std/term"},
            {"kind":"export","loc":{"line":3,"col":1},"symbol":"cprint"}
        ],"files":["io.miva"]}"#;
        let result = from_str(json).unwrap();
        assert_eq!(result.defs.len(), 3);
        assert!(matches!(&result.defs[0], Def::DModule { name, .. } if name == "std.io"));
        assert!(matches!(&result.defs[1], Def::SImport { path, .. } if path == "std/term"));
        assert!(matches!(&result.defs[2], Def::SExport { symbol, .. } if symbol == "cprint"));
    }

    #[test]
    fn test_impl_def() {
        let json = r#"{"defs":[{"kind":"impl","loc":{"line":1,"col":1},"struct":"MyStruct","impls":[{"op":"op_add","func":"my_add","loc":{"line":1,"col":10}}]}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DImpl { struct_name, impls, .. } => {
                assert_eq!(struct_name, "MyStruct");
                assert_eq!(impls.len(), 1);
                assert!(matches!(impls[0].op, ImplOp::ImAdd));
                assert_eq!(impls[0].func, "my_add");
            }
            _ => panic!("expected DImpl"),
        }
    }

    #[test]
    fn test_invalid_json() {
        let result = from_str("not json");
        assert!(result.is_err());
    }

    #[test]
    fn test_wrong_structure() {
        let result = from_str(r#"{"foo":"bar"}"#);
        assert!(result.is_err());
    }

    #[test]
    fn test_struct_lit() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"make_point","params":[],"returns":null,"body":{"kind":"structLit","loc":{"line":2,"col":5},"name":"Point","fields":[{"name":"x","value":{"kind":"int","loc":{"line":2,"col":15},"value":1}},{"name":"y","value":{"kind":"int","loc":{"line":2,"col":22},"value":2}}]},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EStructLit { name, fields, .. } => {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].name, "x");
                    assert!(matches!(fields[0].value, Expr::EInt { value: 1, .. }));
                }
                _ => panic!("expected EStructLit"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_for_loop() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"test_for","params":[],"returns":null,"body":{"kind":"for","loc":{"line":2,"col":5},"var":"i","range":{"kind":"int","loc":{"line":2,"col":15},"value":10},"body":{"kind":"void","loc":{"line":2,"col":20}}},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EFor { var, range, body, .. } => {
                    assert_eq!(var, "i");
                    assert!(matches!(range.as_ref(), Expr::EInt { value: 10, .. }));
                    assert!(matches!(body.as_ref(), Expr::EVoid { .. }));
                }
                _ => panic!("expected EFor"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_import_as() {
        let json = r#"{"defs":[{"kind":"importAs","loc":{"line":1,"col":1},"path":"std/io","alias":"io"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::SImportAs { path, alias, .. } => {
                assert_eq!(path, "std/io");
                assert_eq!(alias, "io");
            }
            _ => panic!("expected SImportAs"),
        }
    }

    #[test]
    fn test_field_access() {
        let json = r#"{"defs":[{"kind":"func","loc":{"line":1,"col":1},"name":"get_x","params":[],"returns":null,"body":{"kind":"fieldAccess","loc":{"line":2,"col":5},"expr":{"kind":"var","loc":{"line":2,"col":5},"name":"p"},"field":"x"},"safety":"safe"}],"files":["test.miva"]}"#;
        let result = from_str(json).unwrap();
        match &result.defs[0] {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EFieldAccess { expr, field, .. } => {
                    assert_eq!(field, "x");
                    assert!(matches!(expr.as_ref(), Expr::EVar { name, .. } if name == "p"));
                }
                _ => panic!("expected EFieldAccess"),
            },
            _ => panic!("expected DFunc"),
        }
    }
}

