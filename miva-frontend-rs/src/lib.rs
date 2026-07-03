pub mod ast;
pub mod lexer;
pub mod parser;
pub mod util;

use lexer::Lexer;
use parser::Parser;

/// Parse Miva source text into a list of definitions.
///
/// `file_name` is used for error reporting and is stored in the output JSON.
pub fn parse(input: &str, file_name: &str) -> Result<Vec<ast::Def>, String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer, input, file_name);
    parser.parse_program()
}

/// Parse and serialize to JSON string.
pub fn parse_to_json(input: &str, file_name: &str) -> Result<String, String> {
    let defs = parse(input, file_name)?;
    let ast_file = ast::AstFile {
        defs,
        files: vec![file_name.to_string()],
    };
    serde_json::to_string_pretty(&ast_file).map_err(|e| format!("JSON error: {}", e))
}

#[cfg(test)]
mod tests {
    use super::ast::*;
    use super::*;

    #[test]
    fn test_parse_empty() {
        let defs = parse("", "test.miva").unwrap();
        assert!(defs.is_empty());
    }

    #[test]
    fn test_parse_module() {
        let defs = parse("module main;", "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_simple_func() {
        let input = "main = () => { return 0; }";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_import() {
        let input = r#"import "std/io";"#;
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_json_output() {
        let json = parse_to_json("", "empty.miva").unwrap();
        assert!(json.contains("\"defs\""));
        assert!(json.contains("\"files\""));
    }

    #[test]
    fn test_parse_struct() {
        let input = "Point = struct { x: int, y: float64 }";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_unsafe_func() {
        let input = "unsafe foo = (): int => { return 42; }";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_arithmetic() {
        let input = "add = (x: int, y: int): int => x + y";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_func_with_body() {
        let input = "main = () => {\n  printlns!(1 + 2, 10 - 3);\n}";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_for_loop() {
        let input = "main = () => {\n  for i in (range(3)) {\n    printlns!(i);\n  };\n}";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_export() {
        let input = "export my_func;";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_import_as() {
        let input = r#"import "std/io" as io;"#;
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_import_here() {
        let input = r#"import "std/io" as .;"#;
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_full_program() {
        let input = r#"
module main;
main = () => {
  println("Hello, World");
}"#;
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 2);
    }

    #[test]
    fn test_parse_generic_func() {
        let input = "identity[T] = (x: T): T => x";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
        match &defs[0] {
            Def::DFunc {
                name,
                type_params,
                params,
                returns,
                ..
            } => {
                assert_eq!(name, "identity");
                assert_eq!(type_params, &vec!["T".to_string()]);
                assert_eq!(params.len(), 1);
                assert!(returns.is_some());
            }
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_parse_generic_call() {
        let input = r#"
module main;
main = () => {
  identity[int](42);
}"#;
        let defs = parse(input, "test.miva").unwrap();
        // Find the identity[int](42) call inside main
        let main_def = &defs[1];
        match main_def {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EBlock { stmts, .. } => match &stmts[0] {
                    Stmt::SExpr { expr, .. } => match expr.as_ref() {
                        Expr::ECall {
                            name,
                            type_args,
                            args,
                            ..
                        } => {
                            assert_eq!(name, "identity");
                            assert_eq!(type_args.len(), 1);
                            assert!(matches!(type_args[0], Typ::TInt));
                            assert_eq!(args.len(), 1);
                        }
                        _ => panic!("expected ECall"),
                    },
                    _ => panic!("expected SExpr"),
                },
                _ => panic!("expected EBlock"),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_parse_generic_multi_param() {
        let input = "pair[T, U] = (a: T, b: U): bool => true";
        let defs = parse(input, "test.miva").unwrap();
        assert_eq!(defs.len(), 1);
        match &defs[0] {
            Def::DFunc {
                name, type_params, ..
            } => {
                assert_eq!(name, "pair");
                assert_eq!(type_params, &vec!["T".to_string(), "U".to_string()]);
            }
            _ => panic!("expected DFunc"),
        }
    }
}
