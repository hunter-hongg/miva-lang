use crate::ast::{Expr, Loc};
use crate::lexer::offset_to_line_col;

/// Process escape sequences in string literals.
/// Supports: \\, \", \n, \t, \r, \xHH, \OOO
/// Uses `.chars()` for correct multi-byte UTF-8 handling.
pub fn process_escapes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('n') => result.push_str("\\n"),
                Some('t') => result.push_str("\\t"),
                Some('r') => result.push_str("\\r"),
                Some('x') => {
                    // \xHH hex escape
                    let hex: String = chars.by_ref().take(2).collect();
                    if hex.len() == 2 {
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(c) = char::from_u32(code) {
                                result.push(c);
                            }
                        }
                    }
                }
                Some(c @ '0'..='9') => {
                    // \OOO octal escape (up to 3 octal digits, 0-7 only)
                    let mut code: u32 = u32::from(c as u8 - b'0');
                    for _ in 0..2 {
                        let peek = chars.clone().next();
                        match peek {
                            Some(d @ '0'..='7') => {
                                code = code * 8 + u32::from(d as u8 - b'0');
                                chars.next();
                            }
                            _ => break,
                        }
                    }
                    if let Some(c) = char::from_u32(code) {
                        result.push(c);
                    }
                }
                Some(other) => {
                    // Other escape: just emit the character
                    result.push(other);
                }
                None => {
                    // Trailing backslash — emit as-is
                    result.push('\\');
                }
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Build an if-elif-else chain expression.
/// OCaml equivalent: build_if_chain
pub fn build_if_chain(
    loc: Loc,
    cond: Expr,
    then_branch: Expr,
    elifs: Vec<(Expr, Expr)>,
    else_opt: Option<Expr>,
) -> Expr {
    // Convert to Box form for internal use — Expr::EIf.else_ is Option<Box<Expr>>
    let mut current_else: Option<Box<Expr>> = else_opt.map(Box::new);

    // Process elifs from right to left (innermost first)
    for (elif_cond, elif_branch) in elifs.into_iter().rev() {
        current_else = Some(Box::new(Expr::EIf {
            loc: loc.clone(),
            cond: Box::new(elif_cond),
            then: Box::new(elif_branch),
            else_: current_else,
        }));
    }

    Expr::EIf {
        loc,
        cond: Box::new(cond),
        then: Box::new(then_branch),
        else_: current_else,
    }
}

/// Convert a dotted ident path like "std.io" into "mvp_std.io" if it starts with "std".
pub fn process_call_path(path: &str) -> String {
    let parts: Vec<&str> = path.splitn(2, '.').collect();
    if parts.len() == 2 && parts[0] == "std" {
        format!("mvp_std.{}", parts[1])
    } else {
        path.to_string()
    }
}

/// Get the head of a slice (for OCaml compat patterns).
pub fn get_head<T>(lst: &[T]) -> Option<&T> {
    lst.first()
}

/// Format a location as "line:col".
pub fn format_loc(loc: &Loc) -> String {
    format!("{}:{}", loc.line, loc.col)
}

/// Convert a byte offset to a Loc (line:col) using the source text.
pub fn loc(input: &str, byte_offset: usize) -> Loc {
    let (line, col) = offset_to_line_col(input, byte_offset);
    Loc::new(line, col)
}

/// Check if a name is snake_case (no uppercase).
pub fn check_snake(loc: &Loc, name: &str, typ: &str) -> Option<String> {
    if name.chars().any(|c| c.is_uppercase()) {
        Some(format!(
            "[W0001] {}:{}:The {} name '{}' isn't a snake_case name.",
            loc.line, loc.col, typ, name
        ))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_escapes_simple() {
        assert_eq!(process_escapes("hello"), "hello");
    }

    #[test]
    fn test_process_escapes_backslash() {
        assert_eq!(process_escapes("a\\\\b"), "a\\b");
    }

    #[test]
    fn test_process_escapes_quote() {
        assert_eq!(process_escapes("a\\\"b"), "a\"b");
    }

    #[test]
    fn test_process_escapes_newline() {
        assert_eq!(process_escapes("a\\nb"), "a\\nb");
    }

    #[test]
    fn test_process_escapes_tab() {
        assert_eq!(process_escapes("a\\tb"), "a\\tb");
    }

    #[test]
    fn test_process_escapes_hex() {
        assert_eq!(process_escapes("\\x41"), "A");
    }

    #[test]
    fn test_process_escapes_octal() {
        assert_eq!(process_escapes("\\101"), "A");
    }

    #[test]
    fn test_process_call_path_std() {
        assert_eq!(process_call_path("std.io"), "mvp_std.io");
        assert_eq!(process_call_path("std.mem.alloc"), "mvp_std.mem.alloc");
    }

    #[test]
    fn test_process_call_path_normal() {
        assert_eq!(process_call_path("my_func"), "my_func");
        assert_eq!(process_call_path("foo.bar"), "foo.bar");
    }

    #[test]
    fn test_build_if_chain() {
        let loc = Loc::new(1, 1);
        let cond = Expr::EBool {
            loc: loc.clone(),
            value: true,
        };
        let then_branch = Expr::EInt {
            loc: loc.clone(),
            value: 1,
        };
        let else_opt = Some(Expr::EInt {
            loc: loc.clone(),
            value: 0,
        });

        let result = build_if_chain(loc.clone(), cond, then_branch, vec![], else_opt);
        match result {
            Expr::EIf {
                cond: c,
                then: t,
                else_: e,
                ..
            } => {
                assert!(matches!(*c, Expr::EBool { value: true, .. }));
                assert!(matches!(*t, Expr::EInt { value: 1, .. }));
                assert!(e.is_some());
            }
            _ => panic!("expected EIf"),
        }
    }

    #[test]
    fn test_check_snake_ok() {
        let loc = Loc::new(1, 1);
        assert!(check_snake(&loc, "snake_case", "function").is_none());
    }

    #[test]
    fn test_check_snake_fail() {
        let loc = Loc::new(1, 1);
        assert!(check_snake(&loc, "CamelCase", "function").is_some());
    }
}
