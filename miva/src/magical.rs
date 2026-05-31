use crate::ast::*;
use crate::warning::Warning;

pub struct MagicalFlag {
    pub warning_offs: Vec<String>,
    pub warning_errs: Vec<String>,
}

impl Default for MagicalFlag {
    fn default() -> Self {
        MagicalFlag {
            warning_offs: vec![],
            warning_errs: vec![],
        }
    }
}

pub fn get_magical_flags(defs: &[Def]) -> MagicalFlag {
    let mut flag = MagicalFlag::default();
    for def in defs {
        if let Def::DCMagical { content, .. } = def {
            let s = content.trim();
            let parts: Vec<&str> = s.splitn(2, ' ').collect();
            if parts.len() < 2 {
                continue;
            }
            match parts[0] {
                "warning_off" => flag.warning_offs.push(parts[1].to_string()),
                "warning_err" => flag.warning_errs.push(parts[1].to_string()),
                _ => {}
            }
        }
    }
    flag
}

pub fn filter_warnings(
    warnings: Vec<Warning>,
    flags: &MagicalFlag,
) -> (Vec<Warning>, Vec<Warning>) {
    let mut simple = vec![];
    let mut errs = vec![];

    'outer: for w in warnings {
        for off in &flags.warning_offs {
            if w.code.starts_with(off) {
                continue 'outer;
            }
        }
        for err in &flags.warning_errs {
            if w.code.starts_with(err) {
                errs.push(w);
                continue 'outer;
            }
        }
        simple.push(w);
    }

    (simple, errs)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Loc {
        Loc { line: 1, col: 1 }
    }

    fn make_cmagical(content: &str) -> Def {
        Def::DCMagical {
            loc: loc(),
            content: content.to_string(),
        }
    }

    fn make_warning(code: &str, msg: &str) -> Warning {
        Warning {
            code: code.to_string(),
            message: msg.to_string(),
        }
    }

    #[test]
    fn test_empty_defs() {
        let flags = get_magical_flags(&[]);
        assert!(flags.warning_offs.is_empty());
        assert!(flags.warning_errs.is_empty());
    }

    #[test]
    fn test_no_magical_defs() {
        let defs = vec![Def::DModule {
            loc: loc(),
            name: "test".to_string(),
        }];
        let flags = get_magical_flags(&defs);
        assert!(flags.warning_offs.is_empty());
        assert!(flags.warning_errs.is_empty());
    }

    #[test]
    fn test_warning_off_collected() {
        let defs = vec![
            make_cmagical("warning_off W0001"),
            make_cmagical("warning_off W0002"),
        ];
        let flags = get_magical_flags(&defs);
        assert_eq!(flags.warning_offs.len(), 2);
        assert!(flags.warning_offs.contains(&"W0001".to_string()));
        assert!(flags.warning_offs.contains(&"W0002".to_string()));
    }

    #[test]
    fn test_warning_err_collected() {
        let defs = vec![make_cmagical("warning_err W0003")];
        let flags = get_magical_flags(&defs);
        assert_eq!(flags.warning_errs.len(), 1);
        assert!(flags.warning_errs.contains(&"W0003".to_string()));
    }

    #[test]
    fn test_release_and_mangle_ignored() {
        let defs = vec![
            make_cmagical("release always"),
            make_cmagical("mangle false"),
        ];
        let flags = get_magical_flags(&defs);
        assert!(flags.warning_offs.is_empty());
        assert!(flags.warning_errs.is_empty());
    }

    #[test]
    fn test_filter_suppresses_matching_warning() {
        let flags = MagicalFlag {
            warning_offs: vec!["W0001".to_string()],
            warning_errs: vec![],
        };
        let warnings = vec![
            make_warning("W0001", "bad name"),
            make_warning("W0002", "deprecated"),
        ];
        let (simple, errs) = filter_warnings(warnings, &flags);
        assert_eq!(simple.len(), 1);
        assert_eq!(simple[0].code, "W0002");
        assert!(errs.is_empty());
    }

    #[test]
    fn test_filter_elevates_to_error() {
        let flags = MagicalFlag {
            warning_offs: vec![],
            warning_errs: vec!["W0002".to_string()],
        };
        let warnings = vec![
            make_warning("W0001", "bad name"),
            make_warning("W0002", "deprecated"),
        ];
        let (simple, errs) = filter_warnings(warnings, &flags);
        assert_eq!(simple.len(), 1);
        assert_eq!(simple[0].code, "W0001");
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].code, "W0002");
    }

    #[test]
    fn test_suppress_takes_priority_over_elevate() {
        let flags = MagicalFlag {
            warning_offs: vec!["W0002".to_string()],
            warning_errs: vec!["W0002".to_string()],
        };
        let warnings = vec![make_warning("W0002", "deprecated")];
        let (simple, errs) = filter_warnings(warnings, &flags);
        assert!(simple.is_empty());
        assert!(errs.is_empty());
    }

    #[test]
    fn test_filter_no_flags() {
        let flags = MagicalFlag::default();
        let warnings = vec![
            make_warning("W0001", "bad name"),
            make_warning("W0002", "deprecated"),
        ];
        let (simple, errs) = filter_warnings(warnings, &flags);
        assert_eq!(simple.len(), 2);
        assert!(errs.is_empty());
    }

    #[test]
    fn test_partial_code_match() {
        let flags = MagicalFlag {
            warning_offs: vec!["W00".to_string()],
            warning_errs: vec![],
        };
        let warnings = vec![
            make_warning("W0001", "bad name"),
            make_warning("W0002", "deprecated"),
            make_warning("W0010", "other"),
        ];
        let (simple, errs) = filter_warnings(warnings, &flags);
        assert!(simple.is_empty());
        assert!(errs.is_empty());
    }

    #[test]
    fn test_mixed_suppress_and_elevate() {
        let flags = MagicalFlag {
            warning_offs: vec!["W0001".to_string()],
            warning_errs: vec!["W0003".to_string()],
        };
        let warnings = vec![
            make_warning("W0001", "bad name"),
            make_warning("W0002", "deprecated"),
            make_warning("W0003", "dangerous"),
            make_warning("W0004", "style"),
        ];
        let (simple, errs) = filter_warnings(warnings, &flags);
        assert_eq!(simple.len(), 2);
        assert_eq!(simple[0].code, "W0002");
        assert_eq!(simple[1].code, "W0004");
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].code, "W0003");
    }
}
