#![allow(dead_code)]

pub const RED: &str = "\x1b[0;31m";
pub const GREEN: &str = "\x1b[0;32m";
pub const YELLOW: &str = "\x1b[0;33m";
pub const BLUE: &str = "\x1b[0;34m";
pub const MAGENTA: &str = "\x1b[0;35m";
pub const CYAN: &str = "\x1b[0;36m";
pub const WHITE: &str = "\x1b[0;37m";
pub const BOLD: &str = "\x1b[1m";
pub const RESET: &str = "\x1b[0m";

pub fn colorize(color: &str, msg: &str) -> String {
    format!("{}{}{}", color, msg, RESET)
}

fn bracket(color: &str, label: &str, msg: &str) -> String {
    format!("  {}[{:>8}]{} {}", color, label, RESET, msg)
}

/// `[   error] message`
pub fn error(msg: &str) -> String {
    bracket(RED, "error", msg)
}

/// `[      ok] message`
pub fn success(msg: &str) -> String {
    bracket(GREEN, "ok", msg)
}

/// `[    info] message`
pub fn info(msg: &str) -> String {
    bracket(CYAN, "info", msg)
}

/// `[ warning] message`
pub fn warn(msg: &str) -> String {
    bracket(YELLOW, "warning", msg)
}

/// `[   label] message` — for compile/link/test progress steps
pub fn step(label: &str, msg: &str) -> String {
    bracket(CYAN, label, msg)
}
