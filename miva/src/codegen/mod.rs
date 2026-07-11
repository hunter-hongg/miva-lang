mod cxx;
mod llvm;
pub mod mvm;

use std::collections::HashMap;
use crate::ast::{Def, Typ};

/// Supported code generation backends.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Backend {
    /// Generate C++ code (default), compiled with g++.
    Cxx,
    /// Generate LLVM IR, compiled with llc.
    Llvm,
    /// Generate MVM bytecode, run with mvm interpreter.
    Mvm,
}

impl Backend {
    pub fn all() -> &'static [Backend] {
        &[Backend::Cxx, Backend::Llvm, Backend::Mvm]
    }

    pub fn name(&self) -> &'static str {
        match self {
            Backend::Cxx => "cxx",
            Backend::Llvm => "llvm",
            Backend::Mvm => "mvm",
        }
    }

    pub fn extension(&self) -> &'static str {
        match self {
            Backend::Cxx => "cpp",
            Backend::Llvm => "ll",
            Backend::Mvm => "mvm",
        }
    }

    pub fn from_name(s: &str) -> Option<Backend> {
        match s {
            "cxx" | "c++" | "cpp" => Some(Backend::Cxx),
            "llvm" | "ll" => Some(Backend::Llvm),
            "mvm" => Some(Backend::Mvm),
            _ => None,
        }
    }
}

/// Output from a code generation backend.
pub struct GeneratedOutput {
    pub program: Vec<u8>,
    pub header: String,
    pub test: String,
    pub extension: &'static str,
}

/// Cross-file function signature: type params + return type.
#[derive(Debug, Clone)]
pub struct FuncSig {
    pub type_params: Vec<String>,
    pub returns: Option<Typ>,
}

/// Extract function signatures from all defs (cross-file) for type-aware codegen.
pub fn collect_func_sigs(defs: &[Def]) -> HashMap<String, FuncSig> {
    let mut sigs = HashMap::new();
    for def in defs {
        match def {
            Def::DFunc { name, type_params, returns, .. } => {
                sigs.entry(name.clone()).or_insert(FuncSig {
                    type_params: type_params.clone(),
                    returns: returns.clone(),
                });
            }
            _ => {}
        }
    }
    sigs
}

/// Convert C-style escape sequences in a string to their actual character values.
pub fn resolve_c_escapes(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('0') => out.push('\0'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some('\'') => out.push('\''),
                Some('x') => {
                    let mut hex = String::with_capacity(2);
                    for _ in 0..2 {
                        match chars.next() {
                            Some(c) if c.is_ascii_hexdigit() => hex.push(c),
                            Some(c) => { hex.push(c); break; },
                            None => break,
                        }
                    }
                    if let Ok(v) = u8::from_str_radix(&hex, 16) {
                        out.push(v as char);
                    } else {
                        out.push('\\');
                        out.push('x');
                        out.push_str(&hex);
                    }
                }
                Some(c) => {
                    out.push('\\');
                    out.push(c);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Generate C++ code from the AST (default backend).
pub fn build_ir(defs: &[Def]) -> [String; 3] {
    cxx::build_ir(defs)
}

/// Generate code from the AST using the specified backend.
pub fn build_ir_with_backend(defs: &[Def], backend: Backend, func_sigs: &HashMap<String, FuncSig>) -> GeneratedOutput {
    match backend {
        Backend::Cxx => {
            let [program, header, test] = cxx::build_ir(defs);
            GeneratedOutput {
                program: program.into_bytes(),
                header,
                test,
                extension: "cpp",
            }
        }
        Backend::Llvm => {
            llvm::build_ir(defs, func_sigs)
        }
        Backend::Mvm => {
            let program = mvm::build_ir(defs);
            let bytes = program.to_bytes();
            GeneratedOutput {
                program: bytes,
                header: String::new(),
                test: String::new(),
                extension: "mvm",
            }
        }
    }
}
