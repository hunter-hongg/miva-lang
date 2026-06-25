#![allow(dead_code)]
use std::time::{SystemTime, UNIX_EPOCH};

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
    colorize(RED, format!("{} ERROR ", now_timestamp()).as_str()).to_string() + msg
}

pub fn infoize(msg: &str) -> String {
    colorize(CYAN, format!("{} INFO ", now_timestamp()).as_str()).to_string() + msg
}

pub fn logize(color: &str, show: &str, msg: &str) -> String {
    colorize(color, format!("{} {} ", now_timestamp(), show).as_str()).to_string() + msg
}

pub fn now_timestamp() -> String {
    let d = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let total_secs = d.as_secs();

    let z = total_secs / 86400 + 719468;
    let era = z / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let dd = doy - (153 * mp + 2) / 5 + 1;
    let mm = if mp < 10 { mp + 3 } else { mp - 9 };
    let yyyy = if mm <= 2 { y + 1 } else { y };

    let time_secs = total_secs % 86400;
    let hh = time_secs / 3600;
    let mi = (time_secs % 3600) / 60;
    let ss = time_secs % 60;

    format!("{:04}{:02}{:02} {:02}:{:02}:{:02}", yyyy, mm, dd, hh, mi, ss)
}
