use std::fmt;

use crate::ast::Loc;

#[derive(Debug, Clone)]
pub struct Error {
    pub code: String,
    pub message: String,
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
            message: format!("[{}:{}] {}", loc.line, loc.col, msg),
        }
    }
}
