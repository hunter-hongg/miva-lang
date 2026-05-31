#![allow(dead_code)]
pub const RED: &str = "\x1b[0;31m";
pub const GREEN: &str = "\x1b[0;32m";
pub const YELLOW: &str = "\x1b[0;33m";
pub const BLUE: &str = "\x1b[0;34m";
pub const MAGENTA: &str = "\x1b[0;35m";
pub const CYAN: &str = "\x1b[0;36m";
pub const WHITE: &str = "\x1b[0;37m";
pub const RESET: &str = "\x1b[0m";

pub fn colorize(color: &str, msg: &str) -> String {
    format!("{}{}{}", color, msg, RESET)
}

pub fn errize(msg: &str) -> String {
    colorize(RED, "Error ").to_string() + msg
}

pub fn infoize(msg: &str) -> String {
    colorize(CYAN, "Info ") .to_string() + msg
}
