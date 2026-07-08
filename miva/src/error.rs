use std::fmt;

use crate::ast::Loc;

#[derive(Debug, Clone)]
pub struct Error {
    pub code: String,
    pub message: String,
    pub loc: Loc,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.code, self.message)
    }
}

impl std::error::Error for Error {}

impl Error {
    pub(crate) fn new(code: &str, loc: &Loc, msg: &str) -> Self {
        Error {
            code: code.to_string(),
            message: msg.to_string(),
            loc: loc.clone(),
        }
    }
}

/// Format a compiler error in Rust-style with source code context.
///
/// Output looks like:
/// ```text
/// error[E0001]: use of moved value x
///  --> src/main.miva:10:5
///   |
/// 10 |     let x = move(x);
///   |             ^^^^^^^
/// ```
pub fn format_error_with_source(err: &Error, file_path: &str, source: &str) -> String {
    let mut output = String::new();
    output.push_str(&format!("error[{}]: {}\n", err.code, err.message));

    let line = err.loc.line;
    let col = err.loc.col;

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
