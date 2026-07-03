use std::collections::HashMap;

use crate::ast::*;
use crate::error::Error;

#[allow(dead_code)]
pub const BUILTIN_FUNCTIONS_COUNT: usize = 23;

const BUILTIN_FUNCTIONS: &[(&str, Safety)] = &[
    ("print", Safety::Safe),
    ("prints", Safety::Safe),
    ("println", Safety::Safe),
    ("printlns", Safety::Safe),
    ("error", Safety::Safe),
    ("errors", Safety::Safe),
    ("errorln", Safety::Safe),
    ("errorlns", Safety::Safe),
    ("exit", Safety::Safe),
    ("abort", Safety::Safe),
    ("panic", Safety::Safe),
    ("string_concat", Safety::Safe),
    ("string_parse", Safety::Safe),
    ("string_length", Safety::Safe),
    ("string_make", Safety::Safe),
    ("string_from", Safety::Safe),
    ("box_new", Safety::Safe),
    ("box_deref", Safety::Safe),
    ("range", Safety::Safe),
    ("ptr_alloc", Safety::Unsafe),
    ("ptr_realloc", Safety::Unsafe),
    ("ptr_free", Safety::Unsafe),
    ("ptr_set", Safety::Unsafe),
];

#[derive(Debug, Clone)]
pub struct FunctionEntry {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<Param>,
    pub return_typ: Option<Typ>,
    #[allow(dead_code)]
    pub safety: Safety,
}

#[derive(Debug, Clone)]
pub struct StructEntry {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub module_name: String,
    pub functions: Vec<FunctionEntry>,
    pub structs: Vec<StructEntry>,
    pub exported_functions: Vec<String>,
    pub files: Vec<String>,
    pub imports: Vec<String>,

    function_index: HashMap<String, usize>,
    struct_index: HashMap<String, usize>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            module_name: String::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            exported_functions: Vec::new(),
            files: Vec::new(),
            imports: Vec::new(),
            function_index: HashMap::new(),
            struct_index: HashMap::new(),
        }
    }

    pub fn build(defs: &[Def]) -> Self {
        let (table, _) = Self::build_with_errors(defs);
        table
    }

    pub fn build_with_errors(defs: &[Def]) -> (Self, Vec<Error>) {
        let mut table = SymbolTable::new();
        let mut errors = Vec::new();

        for def in defs {
            match def {
                Def::DModule { name, .. } => {
                    table.module_name = name.clone();
                }
                Def::DFunc {
                    name,
                    type_params,
                    params,
                    returns,
                    safety,
                    loc,
                    ..
                } => {
                    table.register_function(
                        name,
                        type_params,
                        params,
                        returns,
                        safety,
                        loc,
                        &mut errors,
                    );
                }
                Def::DCFuncUnsafe {
                    name,
                    params,
                    returns,
                    safety,
                    loc,
                    ..
                } => {
                    table.register_function(name, &[], params, returns, safety, loc, &mut errors);
                }
                Def::DStruct { name, fields, loc } => {
                    table.register_struct(name, fields, loc, &mut errors);
                }
                Def::SExport { symbol, .. } => {
                    if table.function_index.contains_key(symbol)
                        && !table.exported_functions.contains(symbol)
                    {
                        table.exported_functions.push(symbol.clone());
                    }
                }
                Def::SImport { path, .. } => {
                    table.files.push(path.clone());
                    table.imports.push(path.clone());
                }
                Def::SImportAs { path, .. } | Def::SImportHere { path, .. } => {
                    table.files.push(path.clone());
                }
                Def::DTest { .. }
                | Def::DMacro { .. }
                | Def::DCMagical { .. }
                | Def::DCIntro { .. }
                | Def::DImpl { .. } => {}
            }
        }

        for (name, safety) in BUILTIN_FUNCTIONS {
            if !table.function_index.contains_key(*name) {
                let idx = table.functions.len();
                table.function_index.insert(name.to_string(), idx);
                table.functions.push(FunctionEntry {
                    name: name.to_string(),
                    type_params: vec![],
                    params: Vec::new(),
                    return_typ: None,
                    safety: safety.clone(),
                });
            }
        }

        (table, errors)
    }

    fn register_function(
        &mut self,
        name: &str,
        type_params: &[String],
        params: &[Param],
        return_typ: &Option<Typ>,
        safety: &Safety,
        loc: &Loc,
        errors: &mut Vec<Error>,
    ) {
        if self.function_index.contains_key(name) {
            errors.push(Error::new(
                "E0004",
                loc,
                &format!("function '{}' is already defined", name),
            ));
        } else {
            let idx = self.functions.len();
            self.function_index.insert(name.to_string(), idx);
        }
        self.functions.push(FunctionEntry {
            name: name.to_string(),
            type_params: type_params.to_vec(),
            params: params.to_vec(),
            return_typ: return_typ.clone(),
            safety: safety.clone(),
        });
    }

    fn register_struct(
        &mut self,
        name: &str,
        fields: &[FieldDef],
        loc: &Loc,
        errors: &mut Vec<Error>,
    ) {
        if self.struct_index.contains_key(name) {
            errors.push(Error::new(
                "E0004",
                loc,
                &format!("struct '{}' is already defined", name),
            ));
        } else {
            let idx = self.structs.len();
            self.struct_index.insert(name.to_string(), idx);
        }
        self.structs.push(StructEntry {
            name: name.to_string(),
            fields: fields.to_vec(),
        });
    }

    pub fn lookup_function(&self, name: &str) -> Option<&FunctionEntry> {
        self.function_index
            .get(name)
            .map(|&idx| &self.functions[idx])
    }

    pub fn lookup_struct(&self, name: &str) -> Option<&StructEntry> {
        self.struct_index.get(name).map(|&idx| &self.structs[idx])
    }

    #[allow(dead_code)]
    pub fn get_function_safety(&self, name: &str) -> Option<Safety> {
        self.lookup_function(name).map(|f| f.safety.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Loc {
        Loc { line: 1, col: 1 }
    }

    fn make_func(name: &str, safety: Safety) -> Def {
        Def::DFunc {
            loc: loc(),
            name: name.to_string(),
            type_params: Vec::new(),
            params: Vec::new(),
            returns: None,
            body: Box::new(Expr::EVoid { loc: loc() }),
            safety,
        }
    }

    fn make_struct(name: &str) -> Def {
        Def::DStruct {
            loc: loc(),
            name: name.to_string(),
            fields: Vec::new(),
        }
    }

    fn make_module(name: &str) -> Def {
        Def::DModule {
            loc: loc(),
            name: name.to_string(),
        }
    }

    fn make_import(path: &str) -> Def {
        Def::SImport {
            loc: loc(),
            path: path.to_string(),
        }
    }

    fn make_import_as(path: &str, alias: &str) -> Def {
        Def::SImportAs {
            loc: loc(),
            path: path.to_string(),
            alias: alias.to_string(),
        }
    }

    fn make_import_here(path: &str) -> Def {
        Def::SImportHere {
            loc: loc(),
            path: path.to_string(),
        }
    }

    fn make_export(symbol: &str) -> Def {
        Def::SExport {
            loc: loc(),
            symbol: symbol.to_string(),
        }
    }

    fn make_test(name: &str) -> Def {
        Def::DTest {
            loc: loc(),
            name: name.to_string(),
            body: Box::new(Expr::EVoid { loc: loc() }),
        }
    }

    fn make_c_func(name: &str) -> Def {
        Def::DCFuncUnsafe {
            loc: loc(),
            name: name.to_string(),
            params: Vec::new(),
            returns: None,
            code: String::new(),
            safety: Safety::Unsafe,
        }
    }

    fn make_cmagical() -> Def {
        Def::DCMagical {
            loc: loc(),
            content: String::new(),
        }
    }

    fn make_cintro() -> Def {
        Def::DCIntro {
            loc: loc(),
            content: String::new(),
        }
    }

    fn make_impl() -> Def {
        Def::DImpl {
            loc: loc(),
            struct_name: "Foo".to_string(),
            impls: Vec::new(),
        }
    }

    #[test]
    fn test_empty_defs() {
        let st = SymbolTable::build(&[]);
        assert!(st.module_name.is_empty());
        assert_eq!(st.functions.len(), BUILTIN_FUNCTIONS_COUNT);
        assert!(st.structs.is_empty());
        assert!(st.exported_functions.is_empty());
        assert!(st.files.is_empty());
        assert!(st.imports.is_empty());
    }

    #[test]
    fn test_module_name() {
        let defs = vec![make_module("std.io")];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.module_name, "std.io");
    }

    #[test]
    fn test_safe_function() {
        let defs = vec![make_func("foo", Safety::Safe)];
        let st = SymbolTable::build(&defs);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT);
        assert_eq!(st.functions[0].name, "foo");
        assert!(matches!(st.functions[0].safety, Safety::Safe));
    }

    #[test]
    fn test_unsafe_function() {
        let defs = vec![make_func("bar", Safety::Unsafe)];
        let st = SymbolTable::build(&defs);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT);
        assert_eq!(st.functions[0].name, "bar");
        assert!(matches!(st.functions[0].safety, Safety::Unsafe));
    }

    #[test]
    fn test_trusted_function() {
        let defs = vec![make_func("baz", Safety::Trusted)];
        let st = SymbolTable::build(&defs);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT);
        assert_eq!(st.functions[0].name, "baz");
        assert!(matches!(st.functions[0].safety, Safety::Trusted));
    }

    #[test]
    fn test_c_function() {
        let defs = vec![make_c_func("my_c_func")];
        let st = SymbolTable::build(&defs);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT);
        assert_eq!(st.functions[0].name, "my_c_func");
        assert!(matches!(st.functions[0].safety, Safety::Unsafe));
    }

    #[test]
    fn test_struct() {
        let defs = vec![make_struct("Point")];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.structs.len(), 1);
        assert_eq!(st.structs[0].name, "Point");
    }

    #[test]
    fn test_module_and_multiple_defs() {
        let defs = vec![
            make_module("myapp"),
            make_func("add", Safety::Safe),
            make_struct("Point"),
            make_func("danger", Safety::Unsafe),
        ];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.module_name, "myapp");
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT + 1);
        assert_eq!(st.structs.len(), 1);
    }

    #[test]
    fn test_export_function() {
        let defs = vec![make_func("foo", Safety::Safe), make_export("foo")];
        let st = SymbolTable::build(&defs);
        assert!(st.exported_functions.contains(&"foo".to_string()));
    }

    #[test]
    fn test_export_nonexistent_function() {
        let defs = vec![make_func("foo", Safety::Safe), make_export("bar")];
        let st = SymbolTable::build(&defs);
        // bar is not a function, so nothing should be exported
        assert!(st.exported_functions.is_empty());
    }

    #[test]
    fn test_import() {
        let defs = vec![make_import("std/io")];
        let st = SymbolTable::build(&defs);
        assert!(st.files.contains(&"std/io".to_string()));
        assert!(st.imports.contains(&"std/io".to_string()));
    }

    #[test]
    fn test_import_as() {
        let defs = vec![make_import_as("std/io", "io")];
        let st = SymbolTable::build(&defs);
        assert!(st.files.contains(&"std/io".to_string()));
    }

    #[test]
    fn test_import_here() {
        let defs = vec![make_import_here("std/io")];
        let st = SymbolTable::build(&defs);
        assert!(st.files.contains(&"std/io".to_string()));
    }

    #[test]
    fn test_function_with_params_and_return() {
        let def = Def::DFunc {
            loc: loc(),
            name: "add".to_string(),
            type_params: vec![],
            params: vec![
                Param::POwn {
                    name: "a".to_string(),
                    typ: Typ::TInt,
                },
                Param::POwn {
                    name: "b".to_string(),
                    typ: Typ::TInt,
                },
            ],
            returns: Some(Typ::TInt),
            body: Box::new(Expr::EVoid { loc: loc() }),
            safety: Safety::Safe,
        };
        let st = SymbolTable::build(&[def]);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT);
        assert_eq!(st.functions[0].params.len(), 2);
        assert!(st.functions[0].return_typ.is_some());
    }

    #[test]
    fn test_struct_with_fields() {
        let def = Def::DStruct {
            loc: loc(),
            name: "Point".to_string(),
            fields: vec![
                FieldDef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                },
                FieldDef {
                    name: "y".to_string(),
                    typ: Typ::TInt,
                },
            ],
        };
        let st = SymbolTable::build(&[def]);
        assert_eq!(st.structs[0].fields.len(), 2);
        assert_eq!(st.structs[0].fields[0].name, "x");
    }

    #[test]
    fn test_test_definition_ignored() {
        let defs = vec![make_test("test_foo")];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.functions.len(), BUILTIN_FUNCTIONS_COUNT);
    }

    #[test]
    fn test_cmagical_ignored() {
        let defs = vec![make_cmagical()];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.functions.len(), BUILTIN_FUNCTIONS_COUNT);
        assert!(st.structs.is_empty());
    }

    #[test]
    fn test_cintro_ignored() {
        let defs = vec![make_cintro()];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.functions.len(), BUILTIN_FUNCTIONS_COUNT);
        assert!(st.structs.is_empty());
    }

    #[test]
    fn test_impl_ignored() {
        let defs = vec![make_impl()];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.functions.len(), BUILTIN_FUNCTIONS_COUNT);
        assert!(st.structs.is_empty());
    }

    #[test]
    fn test_get_function_safety_found() {
        let defs = vec![
            make_func("safe_func", Safety::Safe),
            make_func("unsafe_func", Safety::Unsafe),
            make_func("trusted_func", Safety::Trusted),
        ];
        let st = SymbolTable::build(&defs);
        assert!(matches!(
            st.get_function_safety("safe_func"),
            Some(Safety::Safe)
        ));
        assert!(matches!(
            st.get_function_safety("unsafe_func"),
            Some(Safety::Unsafe)
        ));
        assert!(matches!(
            st.get_function_safety("trusted_func"),
            Some(Safety::Trusted)
        ));
    }

    #[test]
    fn test_get_function_safety_not_found() {
        let defs = vec![make_func("foo", Safety::Safe)];
        let st = SymbolTable::build(&defs);
        assert!(st.get_function_safety("nonexistent").is_none());
    }

    #[test]
    fn test_duplicate_function_error() {
        let defs = vec![
            make_func("foo", Safety::Safe),
            make_func("foo", Safety::Unsafe),
        ];
        let (st, errors) = SymbolTable::build_with_errors(&defs);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT + 1);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, "E0004");
        assert!(errors[0].message.contains("foo"));
        assert!(errors[0].message.contains("already defined"));
    }

    #[test]
    fn test_duplicate_struct_error() {
        let defs = vec![make_struct("Point"), make_struct("Point")];
        let (st, errors) = SymbolTable::build_with_errors(&defs);
        assert_eq!(st.structs.len(), 2);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, "E0004");
        assert!(errors[0].message.contains("Point"));
        assert!(errors[0].message.contains("already defined"));
    }

    #[test]
    fn test_multiple_errors() {
        let defs = vec![
            make_func("foo", Safety::Safe),
            make_func("foo", Safety::Unsafe),
            make_struct("Bar"),
            make_struct("Bar"),
            make_func("baz", Safety::Safe),
        ];
        let (st, errors) = SymbolTable::build_with_errors(&defs);
        assert_eq!(errors.len(), 2);
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT + 2);
        assert_eq!(st.structs.len(), 2);
    }

    #[test]
    fn test_build_without_errors_no_errors() {
        let defs = vec![make_func("foo", Safety::Safe)];
        let (_, errors) = SymbolTable::build_with_errors(&defs);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_new_is_empty() {
        let st = SymbolTable::new();
        assert!(st.module_name.is_empty());
        assert!(st.functions.is_empty());
        assert!(st.structs.is_empty());
        assert!(st.exported_functions.is_empty());
        assert!(st.files.is_empty());
        assert!(st.imports.is_empty());
    }

    #[test]
    fn test_export_before_function_ignored() {
        let defs = vec![make_export("foo"), make_func("foo", Safety::Safe)];
        let st = SymbolTable::build(&defs);
        assert!(
            st.exported_functions.is_empty(),
            "export before function def is silently ignored (OCaml compat)"
        );
    }

    #[test]
    fn test_export_struct_name_ignored() {
        let defs = vec![make_struct("Point"), make_export("Point")];
        let st = SymbolTable::build(&defs);
        assert!(
            st.exported_functions.is_empty(),
            "exporting a struct name should be ignored (OCaml compat)"
        );
    }

    #[test]
    fn test_export_same_function_twice() {
        let defs = vec![
            make_func("foo", Safety::Safe),
            make_export("foo"),
            make_export("foo"),
        ];
        let st = SymbolTable::build(&defs);
        assert_eq!(
            st.exported_functions.len(),
            1,
            "same function exported twice should appear once"
        );
    }

    #[test]
    fn test_last_module_wins() {
        let defs = vec![
            make_module("first"),
            make_module("second"),
            make_module("third"),
        ];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.module_name, "third");
    }

    #[test]
    fn test_struct_no_fields() {
        let def = Def::DStruct {
            loc: loc(),
            name: "Empty".to_string(),
            fields: Vec::new(),
        };
        let st = SymbolTable::build(&[def]);
        assert_eq!(st.structs.len(), 1);
        assert!(st.structs[0].fields.is_empty());
    }

    #[test]
    fn test_function_no_params_no_return() {
        let st = SymbolTable::build(&[make_func("noop", Safety::Safe)]);
        assert_eq!(st.functions[0].params.len(), 0);
        assert!(st.functions[0].return_typ.is_none());
    }

    #[test]
    fn test_def_order_preserved() {
        let defs = vec![
            make_func("z_func", Safety::Safe),
            make_func("a_func", Safety::Safe),
            make_func("m_func", Safety::Safe),
        ];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.functions[0].name, "z_func");
        assert_eq!(st.functions[1].name, "a_func");
        assert_eq!(st.functions[2].name, "m_func");
    }

    #[test]
    fn test_build_consistency() {
        let defs = vec![
            make_module("app"),
            make_func("foo", Safety::Safe),
            make_struct("Point"),
            make_import("std/io"),
        ];
        let (st_with_errs, errs) = SymbolTable::build_with_errors(&defs);
        let st = SymbolTable::build(&defs);
        assert!(errs.is_empty());
        assert_eq!(st.module_name, st_with_errs.module_name);
        assert_eq!(st.functions.len(), st_with_errs.functions.len());
        assert_eq!(st.structs.len(), st_with_errs.structs.len());
    }

    #[test]
    fn test_duplicate_function_three_times_two_errors() {
        let defs = vec![
            make_func("f", Safety::Safe),
            make_func("f", Safety::Unsafe),
            make_func("f", Safety::Trusted),
        ];
        let (_, errors) = SymbolTable::build_with_errors(&defs);
        assert_eq!(errors.len(), 2);
        for err in &errors {
            assert_eq!(err.code, "E0004");
        }
    }

    #[test]
    fn test_mixed_imports_types() {
        let defs = vec![
            make_import("a/b"),
            make_import_as("c/d", "d"),
            make_import_here("e/f"),
        ];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.files.len(), 3);
        assert_eq!(st.imports.len(), 1);
        assert_eq!(st.imports[0], "a/b");
    }

    #[test]
    fn test_empty_string_module_name() {
        let defs = vec![make_module("")];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.module_name, "");
    }

    #[test]
    fn test_export_only_matches_functions_not_cfunctions() {
        let defs = vec![make_c_func("c_get"), make_export("c_get")];
        let st = SymbolTable::build(&defs);
        assert!(
            st.exported_functions.contains(&"c_get".to_string()),
            "DCFuncUnsafe should be exportable like DFunc"
        );
    }

    #[test]
    fn test_round_trip_full() {
        let defs = vec![
            make_module("test_mod"),
            make_func("f1", Safety::Safe),
            make_func("f2", Safety::Unsafe),
            make_struct("S1"),
            make_struct("S2"),
            make_export("f1"),
            make_import("x/y"),
            make_import_as("a/b", "b"),
            make_test("ignored"),
        ];
        let st = SymbolTable::build(&defs);
        assert_eq!(st.module_name, "test_mod");
        assert!(st.functions.len() > BUILTIN_FUNCTIONS_COUNT + 1);
        assert_eq!(st.structs.len(), 2);
        assert_eq!(st.exported_functions, vec!["f1"]);
        assert_eq!(st.files.len(), 2);
        assert_eq!(st.imports.len(), 1);
        assert_eq!(st.imports[0], "x/y");
    }
}
