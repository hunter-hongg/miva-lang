use crate::ast::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

const TARGET_TRIPLE: &str = "x86_64-pc-linux-gnu";

static STR_CONST_COUNTER: AtomicUsize = AtomicUsize::new(0);

static EXTERN_DECLS: Mutex<Option<HashSet<String>>> = Mutex::new(None);

fn module_parts(name: &str) -> Vec<String> {
    let parts: Vec<&str> = name.split('.').collect();
    if parts.first() == Some(&"std") {
        let mut result = vec!["mvp_std".into()];
        result.extend(parts[1..].iter().map(|s| s.to_string()));
        result
    } else {
        // For user-defined modules, use bare function names (the frontend does not
        // prefix calls with the module name, so definitions must match).
        Vec::new()
    }
}

fn make_global_name(module: Option<&str>, func: &str) -> String {
    match module {
        Some(m) => {
            let prefix = module_parts(m);
            let all: Vec<String> = prefix.iter().cloned().chain(std::iter::once(func.to_string())).collect();
            all.join("_")
        }
        None => func.to_string(),
    }
}

fn collect_struct_types(defs: &[Def]) -> Vec<String> {
    let mut types = Vec::new();
    for def in defs {
        match def {
            Def::DStruct { name, fields, .. } => {
                let field_types: Vec<String> = fields.iter().map(|f| {
                    match &f.typ {
                        Typ::TInt => "i64",
                        Typ::TBool | Typ::TChar => "i8",
                        Typ::TFloat64 | Typ::TFloat32 => "double",
                        _ => "i64",
                    }
                }).map(|s| s.to_string()).collect();
                types.push(format!("%{} = type {{ {} }}", name, field_types.join(", ")));
            }
            Def::DModule { .. } => {
                types.extend(collect_struct_types(&defs[1..]));
                break;
            }
            _ => {}
        }
    }
    types
}

fn build_struct_field_map(defs: &[Def]) -> HashMap<String, HashMap<String, usize>> {
    let mut map = HashMap::new();
    for def in defs {
        match def {
            Def::DStruct { name, fields, .. } => {
                let field_idx: HashMap<String, usize> = fields.iter().enumerate()
                    .map(|(i, f)| (f.name.clone(), i))
                    .collect();
                map.insert(name.clone(), field_idx);
            }
            Def::DModule { .. } => {
                for (k, v) in build_struct_field_map(&defs[1..]) {
                    map.entry(k).or_insert(v);
                }
                break;
            }
            _ => {}
        }
    }
    map
}

fn runtime_declarations() -> String {
    let mut decls = String::new();
    decls.push_str("declare void @miva_print(ptr)\n");
    decls.push_str("declare void @miva_println(ptr)\n");
    decls.push_str("declare void @miva_prints(ptr)\n");
    decls.push_str("declare void @miva_printlns(ptr)\n");
    decls.push_str("declare void @miva_error(ptr)\n");
    decls.push_str("declare void @miva_errorln(ptr)\n");
    decls.push_str("declare void @miva_errors(ptr)\n");
    decls.push_str("declare void @miva_errorlns(ptr)\n");
    decls.push_str("declare void @miva_exit(i64)\n");
    decls.push_str("declare void @miva_abort()\n");
    decls.push_str("declare void @miva_panic(ptr)\n");
    decls.push_str("declare ptr @miva_string_concat(ptr, ptr)\n");
    decls.push_str("declare i64 @miva_string_parse(ptr)\n");
    decls.push_str("declare i64 @miva_string_length(ptr)\n");
    decls.push_str("declare ptr @miva_string_make(ptr, i64)\n");
    decls.push_str("declare ptr @miva_string_from_int(i64)\n");
    decls.push_str("declare ptr @miva_string_from_float(double)\n");
    decls.push_str("declare ptr @miva_string_from_bool(i8)\n");
    decls.push_str("declare ptr @miva_string_from_str(ptr)\n");
    decls.push_str("declare void @miva_box_new_int(ptr, i64)\n");
    decls.push_str("declare void @miva_box_new_float(ptr, double)\n");
    decls.push_str("declare void @miva_box_new_bool(ptr, i8)\n");
    decls.push_str("declare void @miva_box_new_byte(ptr, i8)\n");
    decls.push_str("declare void @miva_box_new_string(ptr, ptr)\n");
    decls.push_str("declare i64 @miva_box_deref_int(ptr)\n");
    decls.push_str("declare double @miva_box_deref_float(ptr)\n");
    decls.push_str("declare i8 @miva_box_deref_bool(ptr)\n");
    decls.push_str("declare i8 @miva_box_deref_byte(ptr)\n");
    decls.push_str("declare void @miva_box_deref_string(ptr, ptr)\n");
    decls.push_str("declare void @miva_range(ptr, i64, i64)\n");
    decls.push_str("declare void @miva_range_end(ptr, i64)\n");
    decls.push_str("declare void @miva_range_step(ptr, i64, i64, i64)\n");
    decls.push_str("declare ptr @miva_alloc(i64)\n");
    decls.push_str("declare ptr @miva_realloc(ptr, i64)\n");
    decls.push_str("declare void @miva_free(ptr)\n");
    decls.push_str("declare ptr @miva_ptr_offset(ptr, i64)\n");
    decls.push_str("declare void @miva_ptr_set_i64(ptr, i64)\n");
    decls.push_str("declare void @miva_ptr_set_double(ptr, double)\n");
    decls.push_str("declare void @miva_ptr_set_i8(ptr, i8)\n");
    decls.push_str("declare void @miva_ptr_set_ptr(ptr, ptr)\n");
    decls.push_str("declare i64 @miva_async_await(i64)\n");
    decls.push_str("declare i64 @miva_async_spawn(ptr, i64)\n");
    decls.push_str("declare i64 @miva_json_parse(ptr)\n");
    decls.push_str("declare i64 @miva_json_kind(i64)\n");
    decls.push_str("declare i64 @miva_json_bool(i64)\n");
    decls.push_str("declare i64 @miva_json_number(i64)\n");
    decls.push_str("declare ptr @miva_json_string(i64)\n");
    decls.push_str("declare i64 @miva_json_array_len(i64)\n");
    decls.push_str("declare i64 @miva_json_array_get(i64, i64)\n");
    decls.push_str("declare i64 @miva_json_object_len(i64)\n");
    decls.push_str("declare ptr @miva_json_object_key(i64, i64)\n");
    decls.push_str("declare i64 @miva_json_object_get(i64, i64)\n");
    decls.push_str("declare i64 @miva_json_object_find(i64, ptr)\n");
    decls.push_str("declare void @miva_json_free(i64)\n");
    decls.push_str("declare ptr @miva_json_stringify(i64)\n");
    decls.push_str("declare i64 @miva_xml_parse(ptr)\n");
    decls.push_str("declare i64 @miva_xml_kind(i64)\n");
    decls.push_str("declare ptr @miva_xml_tag(i64)\n");
    decls.push_str("declare i64 @miva_xml_attr_count(i64)\n");
    decls.push_str("declare ptr @miva_xml_attr_name(i64, i64)\n");
    decls.push_str("declare ptr @miva_xml_attr_value(i64, i64)\n");
    decls.push_str("declare ptr @miva_xml_attr_find(i64, ptr)\n");
    decls.push_str("declare i64 @miva_xml_child_count(i64)\n");
    decls.push_str("declare i64 @miva_xml_child_get(i64, i64)\n");
    decls.push_str("declare ptr @miva_xml_text(i64)\n");
    decls.push_str("declare ptr @miva_xml_comment(i64)\n");
    decls.push_str("declare ptr @miva_xml_cdata(i64)\n");
    decls.push_str("declare ptr @miva_xml_pi_target(i64)\n");
    decls.push_str("declare ptr @miva_xml_pi_data(i64)\n");
    decls.push_str("declare ptr @miva_xml_stringify(i64)\n");
    decls.push_str("declare void @miva_xml_free(i64)\n");
    decls.push_str("declare i64 @miva_toml_parse(ptr)\n");
    decls.push_str("declare i64 @miva_toml_kind(i64)\n");
    decls.push_str("declare i64 @miva_toml_bool(i64)\n");
    decls.push_str("declare i64 @miva_toml_number(i64)\n");
    decls.push_str("declare ptr @miva_toml_string(i64)\n");
    decls.push_str("declare i64 @miva_toml_array_len(i64)\n");
    decls.push_str("declare i64 @miva_toml_array_get(i64, i64)\n");
    decls.push_str("declare i64 @miva_toml_object_len(i64)\n");
    decls.push_str("declare ptr @miva_toml_object_key(i64, i64)\n");
    decls.push_str("declare i64 @miva_toml_object_get(i64, i64)\n");
    decls.push_str("declare i64 @miva_toml_object_find(i64, ptr)\n");
    decls.push_str("declare void @miva_toml_free(i64)\n");
    decls.push_str("declare ptr @miva_toml_stringify(i64)\n");
    decls.push_str("declare i64 @miva_yaml_parse(ptr)\n");
    decls.push_str("declare i64 @miva_yaml_kind(i64)\n");
    decls.push_str("declare i64 @miva_yaml_bool(i64)\n");
    decls.push_str("declare i64 @miva_yaml_number(i64)\n");
    decls.push_str("declare ptr @miva_yaml_string(i64)\n");
    decls.push_str("declare i64 @miva_yaml_array_len(i64)\n");
    decls.push_str("declare i64 @miva_yaml_array_get(i64, i64)\n");
    decls.push_str("declare i64 @miva_yaml_object_len(i64)\n");
    decls.push_str("declare ptr @miva_yaml_object_key(i64, i64)\n");
    decls.push_str("declare i64 @miva_yaml_object_get(i64, i64)\n");
    decls.push_str("declare i64 @miva_yaml_object_find(i64, ptr)\n");
    decls.push_str("declare void @miva_yaml_free(i64)\n");
    decls.push_str("declare ptr @miva_yaml_stringify(i64)\n");
    decls.push_str("@.str.void = private unnamed_addr constant [1 x i8] zeroinitializer\n");
    decls
}

fn map_builtin(name: &str, current_module: Option<&str>) -> String {
    match name {
        "print" => "@miva_print".into(),
        "prints" => "@miva_prints".into(),
        "println" => "@miva_println".into(),
        "printlns" => "@miva_printlns".into(),
        "error" => "@miva_error".into(),
        "errors" => "@miva_errors".into(),
        "errorln" => "@miva_errorln".into(),
        "errorlns" => "@miva_errorlns".into(),
        "exit" => "@miva_exit".into(),
        "abort" => "@miva_abort".into(),
        "panic" => "@miva_panic".into(),
        "string_concat" => "@miva_string_concat".into(),
        "string_parse" => "@miva_string_parse".into(),
        "string_length" => "@miva_string_length".into(),
        "string_make" => "@miva_string_make".into(),
        "string_from" => "@miva_string_from_int".into(),
        "box_new" => "@miva_box_new_int".into(),
        "box_deref" => "@miva_box_deref_int".into(),
        "range" => "@miva_range".into(),
        "ptr_alloc" => "@miva_alloc".into(),
        "ptr_realloc" => "@miva_realloc".into(),
        "ptr_free" => "@miva_free".into(),
        "ptr_set" => "@miva_ptr_set_i64".into(),
        "ptr_offset" => "@miva_ptr_offset".into(),
        "await" => "@miva_async_await".into(),
        "json_parse" => "@miva_json_parse".into(),
        "json_kind" => "@miva_json_kind".into(),
        "json_bool" => "@miva_json_bool".into(),
        "json_number" => "@miva_json_number".into(),
        "json_string" => "@miva_json_string".into(),
        "json_array_len" => "@miva_json_array_len".into(),
        "json_array_get" => "@miva_json_array_get".into(),
        "json_object_len" => "@miva_json_object_len".into(),
        "json_object_key" => "@miva_json_object_key".into(),
        "json_object_get" => "@miva_json_object_get".into(),
        "json_object_find" => "@miva_json_object_find".into(),
        "json_free" => "@miva_json_free".into(),
        "json_stringify" => "@miva_json_stringify".into(),
        "xml_parse" => "@miva_xml_parse".into(),
        "xml_kind" => "@miva_xml_kind".into(),
        "xml_tag" => "@miva_xml_tag".into(),
        "xml_attr_count" => "@miva_xml_attr_count".into(),
        "xml_attr_name" => "@miva_xml_attr_name".into(),
        "xml_attr_value" => "@miva_xml_attr_value".into(),
        "xml_attr_find" => "@miva_xml_attr_find".into(),
        "xml_child_count" => "@miva_xml_child_count".into(),
        "xml_child_get" => "@miva_xml_child_get".into(),
        "xml_text" => "@miva_xml_text".into(),
        "xml_comment" => "@miva_xml_comment".into(),
        "xml_cdata" => "@miva_xml_cdata".into(),
        "xml_pi_target" => "@miva_xml_pi_target".into(),
        "xml_pi_data" => "@miva_xml_pi_data".into(),
        "xml_stringify" => "@miva_xml_stringify".into(),
        "xml_free" => "@miva_xml_free".into(),
        "toml_parse" => "@miva_toml_parse".into(),
        "toml_kind" => "@miva_toml_kind".into(),
        "toml_bool" => "@miva_toml_bool".into(),
        "toml_number" => "@miva_toml_number".into(),
        "toml_string" => "@miva_toml_string".into(),
        "toml_array_len" => "@miva_toml_array_len".into(),
        "toml_array_get" => "@miva_toml_array_get".into(),
        "toml_object_len" => "@miva_toml_object_len".into(),
        "toml_object_key" => "@miva_toml_object_key".into(),
        "toml_object_get" => "@miva_toml_object_get".into(),
        "toml_object_find" => "@miva_toml_object_find".into(),
        "toml_free" => "@miva_toml_free".into(),
        "toml_stringify" => "@miva_toml_stringify".into(),
        "yaml_parse" => "@miva_yaml_parse".into(),
        "yaml_kind" => "@miva_yaml_kind".into(),
        "yaml_bool" => "@miva_yaml_bool".into(),
        "yaml_number" => "@miva_yaml_number".into(),
        "yaml_string" => "@miva_yaml_string".into(),
        "yaml_array_len" => "@miva_yaml_array_len".into(),
        "yaml_array_get" => "@miva_yaml_array_get".into(),
        "yaml_object_len" => "@miva_yaml_object_len".into(),
        "yaml_object_key" => "@miva_yaml_object_key".into(),
        "yaml_object_get" => "@miva_yaml_object_get".into(),
        "yaml_object_find" => "@miva_yaml_object_find".into(),
        "yaml_free" => "@miva_yaml_free".into(),
        "yaml_stringify" => "@miva_yaml_stringify".into(),
        _ => {
            let parts: Vec<&str> = name.split('.').collect();
            if parts.first() == Some(&"ffi") {
                format!("@{}", parts[1..].join("_"))
            } else if parts.len() == 1 {
                let full = match current_module {
                    Some(m) => {
                        let mp = module_parts(m);
                        let all: Vec<String> = mp.iter().cloned().chain(std::iter::once(name.to_string())).collect();
                        all.join("_")
                    }
                    None => name.to_string(),
                };
                format!("@{}", full)
            } else {
                format!("@{}", parts.join("_"))
            }
        }
    }
}

struct LlvmCtx {
    indent: usize,
    tmp_counter: usize,
    string_constants: String,
    current_module: Option<String>,
    var_decls: HashMap<String, usize>,
    var_addrs: HashMap<String, String>,
    var_reloads: HashMap<String, String>,
    struct_field_map: HashMap<String, HashMap<String, usize>>,
    /// Flat mapping from field name to numeric index.
    /// Built from all struct definitions. If the same field name appears in
    /// multiple structs at different positions, the first occurrence wins.
    field_idx: HashMap<String, usize>,
    func_sigs: HashMap<String, crate::codegen::FuncSig>,
    string_regs: HashSet<String>,
}

impl LlvmCtx {
    fn new() -> Self {
        LlvmCtx {
            indent: 0,
            tmp_counter: 0,
            string_constants: String::new(),
            current_module: None,
            var_decls: HashMap::new(),
            var_addrs: HashMap::new(),
            var_reloads: HashMap::new(),
            struct_field_map: HashMap::new(),
            field_idx: HashMap::new(),
            func_sigs: HashMap::new(),
            string_regs: HashSet::new(),
        }
    }

    fn with_func_sigs(mut self, sigs: &HashMap<String, crate::codegen::FuncSig>) -> Self {
        self.func_sigs = sigs.clone();
        self
    }

    fn with_module(module: Option<&str>) -> Self {
        LlvmCtx {
            indent: 0,
            tmp_counter: 0,
            string_constants: String::new(),
            current_module: module.map(|m| m.to_string()),
            var_decls: HashMap::new(),
            var_addrs: HashMap::new(),
            var_reloads: HashMap::new(),
            struct_field_map: HashMap::new(),
            field_idx: HashMap::new(),
            func_sigs: HashMap::new(),
            string_regs: HashSet::new(),
        }
    }

    fn with_module_and_fields(module: Option<&str>, struct_field_map: HashMap<String, HashMap<String, usize>>) -> Self {
        let mut field_idx = HashMap::new();
        for (_sname, fields) in &struct_field_map {
            for (fname, fidx) in fields {
                field_idx.entry(fname.clone()).or_insert(*fidx);
            }
        }
        LlvmCtx {
            indent: 0,
            tmp_counter: 0,
            string_constants: String::new(),
            current_module: module.map(|m| m.to_string()),
            var_decls: HashMap::new(),
            var_addrs: HashMap::new(),
            var_reloads: HashMap::new(),
            struct_field_map,
            field_idx,
            func_sigs: HashMap::new(),
            string_regs: HashSet::new(),
        }
    }

    fn gen_tmp(&mut self, prefix: &str) -> String {
        let id = self.tmp_counter;
        self.tmp_counter += 1;
        format!("%{}_{}", prefix, id)
    }

    fn gen_label(&mut self, prefix: &str) -> String {
        let id = self.tmp_counter;
        self.tmp_counter += 1;
        format!("{}_{}", prefix, id)
    }

    fn indent_str(&self) -> String {
        "  ".repeat(self.indent)
    }

    fn declare_var(&mut self, name: &str) -> (String, String) {
        let count = self.var_decls.entry(name.to_string()).or_insert(0);
        let addr = format!("{}.addr.{}", name, count);
        let reload = format!("{}.reload.{}", name, count);
        self.var_addrs.insert(name.to_string(), addr.clone());
        self.var_reloads.insert(name.to_string(), reload.clone());
        *count += 1;
        (addr, reload)
    }

    fn get_var_addr(&self, name: &str) -> String {
        self.var_addrs.get(name).cloned().unwrap_or_else(|| {
            panic!("get_var_addr: variable '{}' not declared", name)
        })
    }

    fn get_var_reload(&self, name: &str) -> String {
        self.var_reloads.get(name).map(|n| format!("%{}", n)).unwrap_or_else(|| {
            panic!("get_var_reload: variable '{}' not declared", name)
        })
    }
}

/// Check if an expression likely evaluates to a string value.
fn is_string_expr(expr: &Expr) -> bool {
    match expr {
        Expr::EString { .. } => true,
        Expr::ECall { name, .. } => {
            matches!(name.as_str(), "string_from" | "string_concat" | "string_make" | "to_string")
        }
        Expr::EBinOp { op: BinOp::Add, left, right, .. } => {
            is_string_expr(left) || is_string_expr(right)
        }
        _ => false,
    }
}

/// Check if an ECall returns a string based on cross-file function signature.
fn call_returns_string(expr: &Expr, ctx: &LlvmCtx) -> bool {
    match expr {
        Expr::ECall { name, type_args, .. } => {
            let lookup = name.rsplit('.').next().unwrap_or(name);
            ctx.func_sigs.get(lookup).map_or(false, |sig| returns_from_sig(sig, type_args))
        }
        _ => false,
    }
}

/// Check if an EVar/EMove refers to a register known to hold a string.
fn is_string_var(expr: &Expr, ctx: &LlvmCtx) -> bool {
    match expr {
        Expr::EVar { name, .. } | Expr::EMove { name, .. } => {
            ctx.var_reloads.get(name).map_or(false, |r| ctx.string_regs.contains(r))
        }
        _ => false,
    }
}

/// Numeric category of a value, used to pick the correct `string_from_*`
/// runtime conversion when stringifying a non-string value.
enum NumKind {
    Int,
    Float,
    Bool,
}

fn expr_numeric_kind(expr: &Expr, ctx: &LlvmCtx) -> NumKind {
    match expr {
        Expr::EFloat { .. } => NumKind::Float,
        Expr::EInt { .. } => NumKind::Int,
        Expr::EBool { .. } => NumKind::Bool,
        Expr::ECall { name, .. } => {
            let lookup = name.rsplit('.').next().unwrap_or(name);
            match ctx.func_sigs.get(lookup) {
                Some(sig) => match &sig.returns {
                    Some(Typ::TFloat64) | Some(Typ::TFloat32) => NumKind::Float,
                    Some(Typ::TBool) => NumKind::Bool,
                    _ => NumKind::Int,
                },
                None => NumKind::Int,
            }
        }
        _ => NumKind::Int,
    }
}

fn emit_fresh_loads(ctx: &mut LlvmCtx, body: &mut String, before: &HashMap<String, String>, names: &[String]) {
    for name in names {
        let addr = ctx.get_var_addr(name);
        let new_reload = format!("{}.reload.{}", name, ctx.tmp_counter);
        ctx.tmp_counter += 1;
        body.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", new_reload, addr));
        ctx.var_reloads.insert(name.clone(), new_reload.clone());
        if before.get(name).map_or(false, |r| ctx.string_regs.contains(r)) {
            ctx.string_regs.insert(new_reload);
        }
    }
}

/// Whether the last non-empty instruction emitted into `body` is a block
/// terminator (a `ret` or `br`). Used to avoid emitting a fall-through
/// `br` (and a malformed merge `phi`) after a branch that already returns.
fn body_ends_in_terminator(body: &str) -> bool {
    for line in body.lines().rev() {
        let t = line.trim();
        if t.is_empty() {
            continue;
        }
        return t.starts_with("ret ") || t.starts_with("br ");
    }
    false
}

fn gen_expr(expr: &Expr, ctx: &mut LlvmCtx, body: &mut String) -> String {
    match expr {
        Expr::EInt { value, .. } => format!("{}", value),
        Expr::EBool { value, .. } => if *value { "1" } else { "0" }.to_string(),
        Expr::EString { value, .. } => {
            let resolved = crate::codegen::resolve_c_escapes(value);
            let id = STR_CONST_COUNTER.fetch_add(1, Ordering::Relaxed);
            let const_name = format!(".str.{}", id);
            let mut llvm_escaped = String::with_capacity(resolved.len());
            for b in resolved.bytes() {
                match b {
                    b'\\' => llvm_escaped.push_str("\\\\"),
                    b'"' => llvm_escaped.push_str("\\22"),
                    0x0A => llvm_escaped.push_str("\\0A"),
                    0x0D => llvm_escaped.push_str("\\0D"),
                    0x09 => llvm_escaped.push_str("\\09"),
                    0x00 => llvm_escaped.push_str("\\00"),
                    0x20..=0x7E => llvm_escaped.push(b as char),
                    _ => llvm_escaped.push_str(&format!("\\{:02X}", b)),
                }
            }
            let len = resolved.len() + 1;
            ctx.string_constants.push_str(&format!(
                "@{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"\n",
                const_name, len, llvm_escaped
            ));
            let ptr_tmp = ctx.gen_tmp("sp");
            body.push_str(&format!(
                "{}{} = getelementptr [{} x i8], ptr @{}, i64 0, i64 0\n",
                ctx.indent_str(), ptr_tmp, len, const_name
            ));
            let call_tmp = ctx.gen_tmp("sc");
            body.push_str(&format!(
                "{}{} = call ptr @miva_string_from_str(ptr {})\n",
                ctx.indent_str(), call_tmp, ptr_tmp
            ));
            let int_tmp = ctx.gen_tmp("si");
            body.push_str(&format!(
                "{}{} = ptrtoint ptr {} to i64\n",
                ctx.indent_str(), int_tmp, call_tmp
            ));
            int_tmp
        }
        Expr::EFloat { value, .. } => {
            let tmp = ctx.gen_tmp("ftmp");
            body.push_str(&format!("{}{} = fadd double 0.0, {}\n", ctx.indent_str(), tmp, value));
            tmp
        }
        Expr::EChar { value, .. } => {
            let c = value.as_bytes().first().copied().unwrap_or(0) as i64;
            format!("{}", c)
        }
        Expr::EBinOp { op, left, right, .. } => {
            let l = gen_expr(left, ctx, body);
            let r = gen_expr(right, ctx, body);
            match op {
                BinOp::Add => {
                    if is_string_expr(left) || is_string_expr(right) {
                        let sp_l = ctx.gen_tmp("sp");
                        let sp_r = ctx.gen_tmp("sp");
                        body.push_str(&format!("{}{} = inttoptr i64 {} to ptr\n", ctx.indent_str(), sp_l, l));
                        body.push_str(&format!("{}{} = inttoptr i64 {} to ptr\n", ctx.indent_str(), sp_r, r));
                        let call_tmp = ctx.gen_tmp("call");
                        body.push_str(&format!("{}{} = call ptr @miva_string_concat(ptr {}, ptr {})\n", ctx.indent_str(), call_tmp, sp_l, sp_r));
                        let int_tmp = ctx.gen_tmp("cr");
                        body.push_str(&format!("{}{} = ptrtoint ptr {} to i64\n", ctx.indent_str(), int_tmp, call_tmp));
                        int_tmp
                    } else {
                        let tmp = ctx.gen_tmp("add");
                        body.push_str(&format!("{}{} = add i64 {}, {}\n", ctx.indent_str(), tmp, l, r));
                        tmp
                    }
                }
                BinOp::Sub => { let tmp = ctx.gen_tmp("sub"); body.push_str(&format!("{}{} = sub i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); tmp }
                BinOp::Mul => { let tmp = ctx.gen_tmp("mul"); body.push_str(&format!("{}{} = mul i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); tmp }
                BinOp::Eq => { let tmp = ctx.gen_tmp("cmp"); body.push_str(&format!("{}{} = icmp eq i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); let tmp2 = ctx.gen_tmp("cmpz"); body.push_str(&format!("{}{} = zext i1 {} to i64\n", ctx.indent_str(), tmp2, tmp)); tmp2 }
                BinOp::Neq => { let tmp = ctx.gen_tmp("cmp"); body.push_str(&format!("{}{} = icmp ne i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); let tmp2 = ctx.gen_tmp("cmpz"); body.push_str(&format!("{}{} = zext i1 {} to i64\n", ctx.indent_str(), tmp2, tmp)); tmp2 }
                BinOp::Lt => { let tmp = ctx.gen_tmp("cmp"); body.push_str(&format!("{}{} = icmp slt i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); let tmp2 = ctx.gen_tmp("cmpz"); body.push_str(&format!("{}{} = zext i1 {} to i64\n", ctx.indent_str(), tmp2, tmp)); tmp2 }
                BinOp::Gt => { let tmp = ctx.gen_tmp("cmp"); body.push_str(&format!("{}{} = icmp sgt i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); let tmp2 = ctx.gen_tmp("cmpz"); body.push_str(&format!("{}{} = zext i1 {} to i64\n", ctx.indent_str(), tmp2, tmp)); tmp2 }
                BinOp::Le => { let tmp = ctx.gen_tmp("cmp"); body.push_str(&format!("{}{} = icmp sle i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); let tmp2 = ctx.gen_tmp("cmpz"); body.push_str(&format!("{}{} = zext i1 {} to i64\n", ctx.indent_str(), tmp2, tmp)); tmp2 }
                BinOp::Ge => { let tmp = ctx.gen_tmp("cmp"); body.push_str(&format!("{}{} = icmp sge i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); let tmp2 = ctx.gen_tmp("cmpz"); body.push_str(&format!("{}{} = zext i1 {} to i64\n", ctx.indent_str(), tmp2, tmp)); tmp2 }
                // Short-circuit logical: LLVM has no native short-circuit icmp;
                // emit `and i1`/`or i1` over the two bool i64 operands (each
                // truncated to i1 first). vec.miva relies on these only inside
                // `if (...)` conditions, so eager evaluation is fine for now.
                BinOp::And => {
                    let tmp = ctx.gen_tmp("and"); body.push_str(&format!("{}{} = and i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); tmp
                }
                BinOp::Or => {
                    let tmp = ctx.gen_tmp("or"); body.push_str(&format!("{}{} = or i64 {}, {}\n", ctx.indent_str(), tmp, l, r)); tmp
                }
            }
        }
        Expr::EIf { cond, then, else_, .. } => {
            let cond_val = gen_expr(cond, ctx, body);
            let cmp = ctx.gen_tmp("ifc");
            let label_then = ctx.gen_label("then");
            let label_else = ctx.gen_label("else");
            let label_end = ctx.gen_label("ifend");
            let var_reloads_before = ctx.var_reloads.clone();
            let var_addrs_before = ctx.var_addrs.clone();
            body.push_str(&format!("{}{} = icmp ne i64 {}, 0\n", ctx.indent_str(), cmp, cond_val));
            body.push_str(&format!("{}br i1 {}, label %{}, label %{}\n", ctx.indent_str(), cmp, label_then, label_else));
            body.push_str(&format!("{}:\n", label_then));
            ctx.indent += 1; let then_val = gen_expr(then, ctx, body); ctx.indent -= 1;
            let var_reloads_after_then = ctx.var_reloads.clone();
            // If the `then` branch already terminates (e.g. it `return`s), do not
            // emit a fall-through `br` and do not list it as a phi predecessor.
            let then_terminated = body_ends_in_terminator(body);
            if !then_terminated {
                body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_end));
            }
            ctx.var_reloads = var_reloads_before.clone();
            ctx.var_addrs = var_addrs_before.clone();
            body.push_str(&format!("{}:\n", label_else));
            ctx.indent += 1; let else_val = if let Some(else_expr) = else_ { gen_expr(else_expr, ctx, body) } else { "0".to_string() }; ctx.indent -= 1;
            let else_terminated = body_ends_in_terminator(body);
            if !else_terminated {
                body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_end));
            }
            // Both branches terminate: the merge block is unreachable, drop it.
            if then_terminated && else_terminated {
                return "0".to_string();
            }
            body.push_str(&format!("{}:\n", label_end));
            let phi_tmp = ctx.gen_tmp("phi");
            // Only non-terminated branches actually reach the merge block.
            let mut phi_entries = String::new();
            if !then_terminated { phi_entries.push_str(&format!("[ {}, %{} ], ", then_val, label_then)); }
            if !else_terminated { phi_entries.push_str(&format!("[ {}, %{} ], ", else_val, label_else)); }
            if !phi_entries.is_empty() {
                // Drop the trailing ", ".
                phi_entries.truncate(phi_entries.trim_end_matches(char::is_whitespace).len());
                phi_entries.truncate(phi_entries.trim_end_matches(',').len());
                body.push_str(&format!("{}{} = phi i64 {}\n", ctx.indent_str(), phi_tmp, phi_entries));
            }
            // Restore var_addrs to pre-if state (branch-scoped allocations don't dominate post-if code)
            ctx.var_addrs = var_addrs_before;
            // Only reload variables that existed before the if (not ones declared inside branches)
            let changed_names: Vec<String> = {
                let vr = &ctx.var_reloads;
                vr.keys().filter(|name| {
                    var_reloads_before.get(*name).is_some() &&
                    (var_reloads_after_then.get(*name) != var_reloads_before.get(*name) ||
                     vr.get(*name) != var_reloads_before.get(*name))
                }).cloned().collect()
            };
            emit_fresh_loads(ctx, body, &var_reloads_before, &changed_names);
            phi_tmp
        }
        Expr::EWhile { cond, body: while_body, .. } => {
            let label_cond = ctx.gen_label("wcond"); let label_body = ctx.gen_label("wbody"); let label_end = ctx.gen_label("wend");
            let var_reloads_before = ctx.var_reloads.clone();
            let var_addrs_before = ctx.var_addrs.clone();
            body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_cond));
            body.push_str(&format!("{}:\n", label_cond));
            // Reload every variable that was live before the loop so that the
            // condition references the latest values from memory (SSA is
            // immutable, so the loop body can only communicate updates through
            // memory). Without these reloads a while-loop whose condition
            // reads a variable that the body writes will loop forever.
            for (name, addr) in &var_addrs_before {
                if var_reloads_before.contains_key(name) {
                    let reload = format!("{}.reloop.{}", name, ctx.tmp_counter);
                    ctx.tmp_counter += 1;
                    body.push_str(&format!("{}%{} = load i64, ptr %{}, align 8\n", ctx.indent_str(), reload, addr));
                    ctx.var_reloads.insert(name.clone(), reload);
                }
            }
            let cond_val = gen_expr(cond, ctx, body); let cmp = ctx.gen_tmp("wc");
            body.push_str(&format!("{}{} = icmp ne i64 {}, 0\n", ctx.indent_str(), cmp, cond_val));
            body.push_str(&format!("{}br i1 {}, label %{}, label %{}\n", ctx.indent_str(), cmp, label_body, label_end));
            body.push_str(&format!("{}:\n", label_body)); ctx.indent += 1; gen_expr(while_body, ctx, body); ctx.indent -= 1;
            body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_cond));
            body.push_str(&format!("{}:\n", label_end));
            // Restore var_addrs to pre-loop state
            ctx.var_addrs = var_addrs_before;
            let changed_names: Vec<String> = {
                let vr = &ctx.var_reloads;
                vr.keys().filter(|name| {
                    var_reloads_before.get(*name).is_some() &&
                    var_reloads_before.get(*name) != vr.get(*name)
                }).cloned().collect()
            };
            emit_fresh_loads(ctx, body, &var_reloads_before, &changed_names);
            "0".to_string()
        }
        Expr::ELoop { body: loop_body, .. } => {
            let label_body = ctx.gen_label("lbody"); let label_end = ctx.gen_label("lend");
            let var_reloads_before = ctx.var_reloads.clone();
            let var_addrs_before = ctx.var_addrs.clone();
            body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_body));
            body.push_str(&format!("{}:\n", label_body)); ctx.indent += 1; gen_expr(loop_body, ctx, body); ctx.indent -= 1;
            body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_body));
            body.push_str(&format!("{}:\n", label_end));
            // Restore var_addrs to pre-loop state
            ctx.var_addrs = var_addrs_before;
            let changed_names: Vec<String> = {
                let vr = &ctx.var_reloads;
                vr.keys().filter(|name| {
                    var_reloads_before.get(*name).is_some() &&
                    var_reloads_before.get(*name) != vr.get(*name)
                }).cloned().collect()
            };
            emit_fresh_loads(ctx, body, &var_reloads_before, &changed_names);
            "0".to_string()
        }
        Expr::EFor { var, range, body: for_body, .. } => {
            // Extract the count from `range(n)` — the range builtin returns void
            // in LLVM (writes through an output pointer), so we evaluate the
            // argument of range() directly as the loop bound.
            let range_count = match range.as_ref() {
                Expr::ECall { name, args, .. } if name == "range" && args.len() == 1 => {
                    gen_expr(&args[0], ctx, body)
                }
                _ => gen_expr(range, ctx, body),
            };
            let label_cond = ctx.gen_label("fcond"); let label_body = ctx.gen_label("fbody"); let label_end = ctx.gen_label("fend");
            let var_reloads_before = ctx.var_reloads.clone();
            let var_addrs_before = ctx.var_addrs.clone();
            // Declare the loop variable (creates an alloca address)
            let (addr, _reload) = ctx.declare_var(var);
            body.push_str(&format!("  %{} = alloca i64, align 8\n", addr));
            // Initialize loop counter to 0
            body.push_str(&format!("  store i64 0, ptr %{}, align 8\n", addr));
            body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_cond));
            body.push_str(&format!("{}:\n", label_cond));
            // Reload loop counter
            let reload_name = format!("{}.fv.{}", var, ctx.tmp_counter);
            ctx.tmp_counter += 1;
            body.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", reload_name, addr));
            ctx.var_reloads.insert(var.clone(), reload_name.clone());
            let cmp = ctx.gen_tmp("fc");
            body.push_str(&format!("{}{} = icmp slt i64 %{}, {}\n", ctx.indent_str(), cmp, reload_name, range_count));
            body.push_str(&format!("{}br i1 {}, label %{}, label %{}\n", ctx.indent_str(), cmp, label_body, label_end));
            body.push_str(&format!("{}:\n", label_body)); ctx.indent += 1; gen_expr(for_body, ctx, body); ctx.indent -= 1;
            // Increment and store loop counter back
            let next_name = format!("{}.fn.{}", var, ctx.tmp_counter);
            ctx.tmp_counter += 1;
            body.push_str(&format!("  %{} = add i64 %{}, 1\n", next_name, reload_name));
            body.push_str(&format!("  store i64 %{}, ptr %{}, align 8\n", next_name, addr));
            body.push_str(&format!("{}br label %{}\n", ctx.indent_str(), label_cond));
            body.push_str(&format!("{}:\n", label_end));
            // Loop variable is out of scope after the loop
            ctx.var_reloads.remove(var);
            // Restore var_addrs to pre-loop state (loop-scoped allocations don't dominate post-loop code)
            ctx.var_addrs = var_addrs_before;
            // Only reload variables that existed before the loop (not ones declared inside)
            let changed_names: Vec<String> = {
                let vr = &ctx.var_reloads;
                vr.keys().filter(|name| {
                    var_reloads_before.get(*name).is_some() &&
                    var_reloads_before.get(*name) != vr.get(*name)
                }).cloned().collect()
            };
            emit_fresh_loads(ctx, body, &var_reloads_before, &changed_names);
            "0".to_string()
        }
        Expr::EVar { name, .. } | Expr::EMove { name, .. } | Expr::EClone { name, .. } => ctx.get_var_reload(name),
        Expr::EVoid { .. } => "0".to_string(),
        Expr::ECall { name, type_args, args, .. } => gen_call(name, args, type_args, ctx, body),
        Expr::EMethodCall { method, type_args, args, .. } => gen_call(method, args, type_args, ctx, body),
        Expr::ECast { expr, to, .. } => {
            let val = gen_expr(expr, ctx, body);
            match to {
                Typ::TFloat64 => { let tmp = ctx.gen_tmp("cast"); body.push_str(&format!("{}{} = sitofp i64 {} to double\n", ctx.indent_str(), tmp, val)); tmp }
                Typ::TFloat32 => { let tmp = ctx.gen_tmp("cast"); body.push_str(&format!("{}{} = sitofp i64 {} to float\n", ctx.indent_str(), tmp, val)); tmp }
                _ => val,
            }
        }
        Expr::EBlock { stmts, result, .. } => {
            for stmt in stmts { gen_stmt(stmt, ctx, body); }
            match result { Some(r) => gen_expr(r, ctx, body), None => "0".to_string() }
        }
        Expr::EStructLit { name: struct_name, fields, .. } => {
            let tmp = ctx.gen_tmp("st");
            let size = fields.len() * 8;
            body.push_str(&format!("{}{} = call ptr @miva_alloc(i64 {})\n", ctx.indent_str(), tmp, size));
            for (i, f) in fields.iter().enumerate() {
                let fv = gen_expr(&f.value, ctx, body);
                let gep = ctx.gen_tmp("sf");
                body.push_str(&format!("{}{} = getelementptr i8, ptr {}, i64 {}\n", ctx.indent_str(), gep, tmp, i * 8));
                let gep_typed = ctx.gen_tmp("sf");
                body.push_str(&format!("{}{} = bitcast ptr {} to ptr\n", ctx.indent_str(), gep_typed, gep));
                body.push_str(&format!("{}store i64 {}, ptr {}\n", ctx.indent_str(), fv, gep_typed));
            }
            let int_tmp = ctx.gen_tmp("stint");
            body.push_str(&format!("{}{} = ptrtoint ptr {} to i64\n", ctx.indent_str(), int_tmp, tmp));
            int_tmp
        }
        Expr::EFieldAccess { expr: fexpr, field, .. } => {
            let val = gen_expr(fexpr, ctx, body);
            let ptr_val = ctx.gen_tmp("fa_ptr");
            body.push_str(&format!("{}{} = inttoptr i64 {} to ptr\n", ctx.indent_str(), ptr_val, val));
            let field_idx = ctx.field_idx.get(field).copied().unwrap_or(0);
            let gep = ctx.gen_tmp("fa");
            body.push_str(&format!("{}{} = getelementptr i64, ptr {}, i64 {}\n", ctx.indent_str(), gep, ptr_val, field_idx));
            let load = ctx.gen_tmp("fal");
            body.push_str(&format!("{}{} = load i64, ptr {}\n", ctx.indent_str(), load, gep));
            load
        }
        Expr::EChoose { .. } => "0".to_string(),
        Expr::EArrayLit { .. } => "0".to_string(),
        Expr::EAddr { expr: aexpr, .. } => gen_expr(aexpr, ctx, body),
        Expr::EDeref { expr: dexpr, .. } => {
            // Pointer values are modelled as i64 addresses in the LLVM backend,
            // so `gen_expr(dexpr)` yields an i64 holding the address. Convert it
            // back to a real pointer before dereferencing.
            let val = gen_expr(dexpr, ctx, body);
            let ptr_tmp = ctx.gen_tmp("deref_ptr");
            body.push_str(&format!("{}{} = inttoptr i64 {} to ptr\n", ctx.indent_str(), ptr_tmp, val));
            let tmp = ctx.gen_tmp("deref");
            body.push_str(&format!("{}{} = load i64, ptr {}\n", ctx.indent_str(), tmp, ptr_tmp));
            tmp
        }
        Expr::EMacro { .. } | Expr::EMacroVar { .. } => "0".to_string(),
    }
}

/// Whether the i-th argument of a runtime function should be passed as `ptr`
/// instead of `i64`. Used to convert string handles back to pointers.
fn is_ptr_arg(func_name: &str, arg_idx: usize) -> bool {
    match func_name {
        n if n == "@miva_panic" => true,
        n if n.contains("miva_print") || n.contains("miva_error") => true,
        "@miva_string_concat" | "@miva_string_from_str" => true,
        "@miva_string_parse" | "@miva_string_length" => true,
        "@miva_string_make" => arg_idx == 0,
        // @miva_box_new_string takes ptr for both args (the box ptr and the string value)
        "@miva_box_new_string" => arg_idx == 0 || arg_idx == 1,
        "@miva_box_new_int" | "@miva_box_new_float" | "@miva_box_new_bool"
        | "@miva_box_new_byte" => arg_idx == 0,
        "@miva_box_deref_int" | "@miva_box_deref_float" | "@miva_box_deref_bool"
        | "@miva_box_deref_byte" => false,
        "@miva_box_deref_string" => true,
        "@miva_range" | "@miva_range_end" | "@miva_range_step" => arg_idx == 0,
        "@miva_json_parse" => arg_idx == 0,
        "@miva_json_object_find" => arg_idx == 1,
        "@miva_xml_parse" => arg_idx == 0,
        "@miva_xml_attr_find" => arg_idx == 1,
        "@miva_toml_parse" => arg_idx == 0,
        "@miva_toml_object_find" => arg_idx == 1,
        "@miva_yaml_parse" => arg_idx == 0,
        "@miva_yaml_object_find" => arg_idx == 1,
        // Pointer-manipulation builtins (all take ptr as first arg)
        "@miva_realloc" | "@miva_free" | "@miva_ptr_offset" => arg_idx == 0,
        "@miva_ptr_set_i64" | "@miva_ptr_set_double" | "@miva_ptr_set_i8" | "@miva_ptr_set_ptr" => arg_idx == 0,
        "@miva_async_spawn" => arg_idx == 0,
        _ => false,
    }
}

fn ret_type(func_name: &str) -> &'static str {
    match func_name {
        n if n.contains("miva_print") || n.contains("miva_error") || n == "@miva_panic"
            || n == "@miva_abort" || n == "@miva_exit" => "void",
        n if n == "@miva_string_concat" || n == "@miva_string_make"
            || n.starts_with("@miva_string_from_") || n == "@miva_alloc" || n == "@miva_realloc"
            || n == "@miva_json_string" || n == "@miva_json_object_key"
            || n == "@miva_json_stringify" => "ptr",
        n if n == "@miva_xml_tag" || n == "@miva_xml_attr_name"
            || n == "@miva_xml_attr_value" || n == "@miva_xml_attr_find"
            || n == "@miva_xml_text" || n == "@miva_xml_comment"
            || n == "@miva_xml_cdata" || n == "@miva_xml_pi_target"
            || n == "@miva_xml_pi_data" || n == "@miva_xml_stringify" => "ptr",
        n if n == "@miva_toml_string" || n == "@miva_toml_object_key"
            || n == "@miva_toml_stringify" => "ptr",
        n if n == "@miva_yaml_string" || n == "@miva_yaml_object_key"
            || n == "@miva_yaml_stringify" => "ptr",
        n if n == "@miva_box_deref_float" => "double",
        n if n == "@miva_box_deref_bool" || n == "@miva_box_deref_byte" => "i8",
        // Pointer-manipulation builtins
        "@miva_ptr_offset" => "ptr",
        "@miva_free" | "@miva_ptr_set_i64" | "@miva_ptr_set_double" | "@miva_ptr_set_i8" | "@miva_ptr_set_ptr" => "void",
        n if n.starts_with("@miva_") || n.starts_with("@ffi_") => "i64",
        _ => "i64",
    }
}

fn gen_call(name: &str, args: &[Expr], type_args: &[Typ], ctx: &mut LlvmCtx, body: &mut String) -> String {
        // string_from/to_string on already-string arg: skip conversion, pass through.
    // When the arg is NOT a string, still emit the conversion using the already
    // computed register — never re-evaluate args[0], which would double-evaluate
    // side-effecting expressions such as a second `await` on the same future.
    if (name == "string_from" || name == "to_string") && args.len() == 1 {
        let reg = gen_expr(&args[0], ctx, body);
        let is_str = is_string_expr(&args[0]) || call_returns_string(&args[0], ctx)
            || is_string_var(&args[0], ctx) || ctx.string_regs.contains(&reg);
        if is_str {
            return reg;
        }
        // The value is numeric; choose the matching runtime conversion so the
        // result stringifies correctly (e.g. 0.1 not the raw bit-pattern).
        let tmp = ctx.gen_tmp("call");
        let (fn_name, arg_llvm) = match expr_numeric_kind(&args[0], ctx) {
            NumKind::Float => {
                let double_reg = if matches!(args[0], Expr::EFloat { .. }) {
                    reg
                } else {
                    let bc = ctx.gen_tmp("bc");
                    body.push_str(&format!("{}{} = bitcast i64 {} to double\n", ctx.indent_str(), bc, reg));
                    bc
                };
                ("@miva_string_from_float".to_string(), format!("double {}", double_reg))
            }
            NumKind::Bool => {
                let b = ctx.gen_tmp("bt");
                body.push_str(&format!("{}{} = trunc i64 {} to i8\n", ctx.indent_str(), b, reg));
                ("@miva_string_from_bool".to_string(), format!("i8 {}", b))
            }
            NumKind::Int => ("@miva_string_from_int".to_string(), format!("i64 {}", reg)),
        };
        body.push_str(&format!("{} = call ptr {}({})\n",
            format!("{}{}", ctx.indent_str(), tmp), fn_name, arg_llvm));
        let int_tmp = ctx.gen_tmp("cr");
        body.push_str(&format!("{}{} = ptrtoint ptr {} to i64\n", ctx.indent_str(), int_tmp, tmp));
        ctx.string_regs.insert(int_tmp.clone());
        return int_tmp;
    }

    // `await(f)` / `f.await()`: join the spawned task and return its value.
    if name == "await" {
        let arg = match args.first() {
            Some(a) => a,
            None => return String::new(),
        };
        let arg_reg = gen_expr(arg, ctx, body);
        let tmp = ctx.gen_tmp("call");
        body.push_str(&format!("{} = call i64 @miva_async_await(i64 {})\n",
            format!("{}{}", ctx.indent_str(), tmp), arg_reg));
        // The awaited value is the future's inner type; mirror string-ness.
        if call_returns_string(arg, ctx) || is_string_expr(arg) || is_string_var(arg, ctx) {
            ctx.string_regs.insert(tmp.clone());
        }
        return tmp;
    }

    // Spawn a real OS thread for calls to async functions. The async function
    // takes a single `i64` (pointer to a heap struct of packed args); the
    // runtime spawns it and returns a task handle (also an i64).
    let lookup = name.rsplit('.').next().unwrap_or(name);
    let is_async_call = ctx.func_sigs.get(lookup).map_or(false, |s| s.is_async);
    if is_async_call {
        let arg_regs: Vec<String> = args.iter()
            .map(|a| gen_expr(a, ctx, body))
            .collect();
        let struct_bytes = (arg_regs.len() as i64) * 8;
        let struct_ptr = ctx.gen_tmp("asp");
        body.push_str(&format!("{}{} = call ptr @miva_alloc(i64 {})\n",
            ctx.indent_str(), struct_ptr, struct_bytes));
        for (i, reg) in arg_regs.iter().enumerate() {
            let gep = ctx.gen_tmp("asg");
            body.push_str(&format!("{}{} = getelementptr i64, ptr {}, i64 {}\n",
                ctx.indent_str(), gep, struct_ptr, i));
            body.push_str(&format!("{}store i64 {}, ptr {}\n", ctx.indent_str(), reg, gep));
        }
        let struct_int = ctx.gen_tmp("asi");
        body.push_str(&format!("{}{} = ptrtoint ptr {} to i64\n",
            ctx.indent_str(), struct_int, struct_ptr));
        let func_name = map_builtin(name, ctx.current_module.as_deref());
        let handle = ctx.gen_tmp("call");
        body.push_str(&format!("{} = call i64 @miva_async_spawn(ptr {}, i64 {})\n",
            format!("{}{}", ctx.indent_str(), handle), func_name, struct_int));
        // The handle is a plain i64; it is NOT a string. String-ness is applied
        // by the `await` that later joins this handle.
        return handle;
    }

    let func_name = map_builtin(name, ctx.current_module.as_deref());
    if !func_name.starts_with("@miva_") && !func_name.starts_with("@ffi_") {
        if let Ok(mut guard) = EXTERN_DECLS.lock() {
            guard.get_or_insert_with(HashSet::new).insert(func_name.trim_start_matches('@').to_string());
        }
    }

    let mut arg_strs = Vec::new();
    for (i, a) in args.iter().enumerate() {
        let t = gen_expr(a, ctx, body);
        if is_ptr_arg(&func_name, i) {
            let ptr_val = ctx.gen_tmp("sp");
            body.push_str(&format!("{}{} = inttoptr i64 {} to ptr\n", ctx.indent_str(), ptr_val, t));
            arg_strs.push(format!("ptr {}", ptr_val));
        } else {
            arg_strs.push(format!("i64 {}", t));
        }
    }
    let ret_ty = ret_type(&func_name);
    if ret_ty == "void" {
        body.push_str(&format!("{}call void {}({})\n", ctx.indent_str(), func_name, arg_strs.join(", ")));
        "0".to_string()
    } else {
        let tmp = ctx.gen_tmp("call");
        body.push_str(&format!("{}{} = call {} {}({})\n", ctx.indent_str(), tmp, ret_ty, func_name, arg_strs.join(", ")));
        let result = if ret_ty == "ptr" {
            let int_tmp = ctx.gen_tmp("cr");
            body.push_str(&format!("{}{} = ptrtoint ptr {} to i64\n", ctx.indent_str(), int_tmp, tmp));
            // Mark ptr-to-i64 converted register as string
            ctx.string_regs.insert(int_tmp.clone());
            int_tmp
        } else {
            // For user functions, check signature to see if result is a string
            let lookup = name.rsplit('.').next().unwrap_or(name);
            if !func_name.starts_with("@miva_") && !func_name.starts_with("@ffi_") {
                if let Some(sig) = ctx.func_sigs.get(lookup) {
                    if returns_from_sig(sig, type_args) {
                        ctx.string_regs.insert(tmp.clone());
                    }
                }
            }
            tmp
        };
        result
    }
}

/// Check if a FuncSig indicates string return given concrete type_args.
fn returns_from_sig(sig: &crate::codegen::FuncSig, type_args: &[Typ]) -> bool {
    match &sig.returns {
        Some(Typ::TString) => true,
        Some(Typ::TFuture { of }) => {
            if let Typ::TString = **of {
                return true;
            }
            false
        }
        Some(Typ::TStruct { name, .. }) => {
            if let Some(pos) = sig.type_params.iter().position(|p| p == name) {
                if pos < type_args.len() {
                    return matches!(&type_args[pos], Typ::TString);
                }
            }
            false
        }
        _ => false,
    }
}

fn gen_stmt(stmt: &Stmt, ctx: &mut LlvmCtx, body: &mut String) {
    match stmt {
        Stmt::SLet { name, mutable: _, expr, .. } => {
            let val = gen_expr(expr, ctx, body);
            let (addr, reload) = ctx.declare_var(name);
            body.push_str(&format!("  %{} = alloca i64, align 8\n", addr));
            body.push_str(&format!("  store i64 {}, ptr %{}, align 8\n", val, addr));
            body.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", reload, addr));
            if is_string_expr(expr) || call_returns_string(expr, ctx) {
                ctx.string_regs.insert(reload);
            }
        }
        Stmt::SLetTyped { name, expr, .. } => {
            let val = gen_expr(expr, ctx, body);
            let (addr, reload) = ctx.declare_var(name);
            body.push_str(&format!("  %{} = alloca i64, align 8\n", addr));
            body.push_str(&format!("  store i64 {}, ptr %{}, align 8\n", val, addr));
            body.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", reload, addr));
            if is_string_expr(expr) || call_returns_string(expr, ctx) {
                ctx.string_regs.insert(reload);
            }
        }
        Stmt::SAssign { name, expr, .. } => {
            let val = gen_expr(expr, ctx, body);
            let addr = ctx.get_var_addr(name);
            body.push_str(&format!("  store i64 {}, ptr %{}, align 8\n", val, addr));
            let reload_name = format!("{}.reload.{}", name, ctx.tmp_counter);
            ctx.tmp_counter += 1;
            body.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", reload_name, addr));
            ctx.var_reloads.insert(name.clone(), reload_name.clone());
            if is_string_expr(expr) || call_returns_string(expr, ctx) {
                ctx.string_regs.insert(reload_name);
            }
        }
        Stmt::SReturn { expr, .. } => {
            let val = gen_expr(expr, ctx, body);
            body.push_str(&format!("  ret i64 {}\n", val));
        }
        Stmt::SFieldAssign { target, field, expr, .. } => {
            // `target.field = expr` — LLVM backend field write. Compute the
            // base struct pointer (i64), GEP to the field's offset, store the
            // value. Field indices come from the per-module struct field map
            // in `ctx` (same source as EFieldAccess reads).
            let base = gen_expr(target, ctx, body);
            let val = gen_expr(expr, ctx, body);
            let field_idx = ctx.field_idx.get(field.as_str()).copied().unwrap_or(0);
            let ptr_tmp = ctx.gen_tmp("fa");
            body.push_str(&format!("  {} = inttoptr i64 {} to ptr\n", ptr_tmp, base));
            let gep = ctx.gen_tmp("g");
            body.push_str(&format!("  {} = getelementptr i64, ptr {}, i64 {}\n", gep, ptr_tmp, field_idx));
            body.push_str(&format!("  store i64 {}, ptr {}\n", val, gep));
        }
        Stmt::SExpr { expr, .. } => { gen_expr(expr, ctx, body); }
        Stmt::SCIntro { content, .. } => { body.push_str(&format!("  ; {}\n", content)); }
        Stmt::SEmpty { .. } => {}
    }
}

fn gen_func_def(
    name: &str, _type_params: &[String], params: &[Param], _returns: &Option<Typ>,
    body: &Expr, module: Option<&str>, struct_field_map: &HashMap<String, HashMap<String, usize>>,
    func_sigs: &HashMap<String, crate::codegen::FuncSig>, is_async: bool,
) -> String {
    let global_name = make_global_name(module, name);
    let mut ctx = LlvmCtx::with_module_and_fields(module, struct_field_map.clone()).with_func_sigs(func_sigs);
    ctx.indent = 1;
    let mut body_prefix = String::new();
    let param_strs: Vec<String>;
    if is_async {
        // Async functions take a single i64 that is a pointer to a heap struct
        // holding the packed arguments. The runtime spawns them on a thread.
        param_strs = vec!["i64 %args".to_string()];
        body_prefix.push_str("  %args_ptr = inttoptr i64 %args to ptr\n");
        for (i, p) in params.iter().enumerate() {
            let pname = match p { Param::PRef { name, .. } | Param::POwn { name, .. } => name.as_str() };
            let (addr, reload) = ctx.declare_var(pname);
            let gep = ctx.gen_tmp("ag");
            body_prefix.push_str(&format!("  {} = getelementptr i64, ptr %args_ptr, i64 {}\n", gep, i));
            let val = ctx.gen_tmp("av");
            body_prefix.push_str(&format!("  {} = load i64, ptr {}\n", val, gep));
            body_prefix.push_str(&format!("  %{} = alloca i64, align 8\n", addr));
            body_prefix.push_str(&format!("  store i64 {}, ptr %{}, align 8\n", val, addr));
            body_prefix.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", reload, addr));
        }
    } else {
        param_strs = params.iter().map(|p| {
            let pname = match p { Param::PRef { name, .. } | Param::POwn { name, .. } => name.as_str() };
            format!("i64 %{}", pname)
        }).collect();
        for p in params {
            let pname = match p { Param::PRef { name, .. } | Param::POwn { name, .. } => name.as_str() };
            let (addr, reload) = ctx.declare_var(pname);
            body_prefix.push_str(&format!("  %{} = alloca i64, align 8\n", addr));
            body_prefix.push_str(&format!("  store i64 %{}, ptr %{}, align 8\n", pname, addr));
            body_prefix.push_str(&format!("  %{} = load i64, ptr %{}, align 8\n", reload, addr));
        }
    }
    let mut body_str = String::new();
    // Implicit return: for non-void functions whose body is a block with no
    // explicit result expression, the last SExpr is treated as the block value
    // (matching CXX's take_last_expr in cxx_func_inner).  Without this a
    // function like `{ ptr_alloc(n); }` returns 0 instead of the allocation.
    let ret_val = if _returns.is_some() {
        if let Expr::EBlock { stmts, result: None, .. } = body {
            if let Some((Stmt::SExpr { expr, .. }, leading)) = stmts.split_last() {
                for s in leading {
                    gen_stmt(s, &mut ctx, &mut body_str);
                }
                gen_expr(expr.as_ref(), &mut ctx, &mut body_str)
            } else {
                gen_expr(body, &mut ctx, &mut body_str)
            }
        } else {
            gen_expr(body, &mut ctx, &mut body_str)
        }
    } else {
        gen_expr(body, &mut ctx, &mut body_str)
    };
    let mut out = String::new();
    out.push_str(&ctx.string_constants);
    out.push_str(&format!("define i64 @{}({}) {{\n", global_name, param_strs.join(", ")));
    out.push_str("entry:\n");
    out.push_str(&body_prefix);
    out.push_str(&body_str);
    out.push_str(&format!("  ret i64 {}\n", ret_val));
    out.push_str("}\n\n");
    out
}

fn gen_main_func(body_expr: &Expr, struct_field_map: &HashMap<String, HashMap<String, usize>>, func_sigs: &HashMap<String, crate::codegen::FuncSig>) -> String {
    let mut ctx = LlvmCtx::with_module_and_fields(None, struct_field_map.clone()).with_func_sigs(func_sigs);
    ctx.indent = 1;
    let (argc_addr, _argc_reload) = ctx.declare_var("argc");
    let mut body = String::new();
    body.push_str(&format!("  %{} = alloca i64, align 8\n", argc_addr));
    body.push_str(&format!("  store i64 %argc, ptr %{}, align 8\n", argc_addr));
    let ret_val = gen_expr(body_expr, &mut ctx, &mut body);
    let mut out = String::new();
    out.push_str(&ctx.string_constants);
    out.push_str("define i64 @mvp_own_main(i64 %argc) {\n");
    out.push_str("entry:\n");
    out.push_str(&body);
    out.push_str(&format!("  ret i64 {}\n", ret_val));
    out.push_str("}\n\n");
    out.push_str("define i32 @main(i32 %argc, ptr %argv) {\n");
    out.push_str("entry:\n");
    out.push_str("  %ext = sext i32 %argc to i64\n");
    out.push_str("  %ret = call i64 @mvp_own_main(i64 %ext)\n");
    out.push_str("  ret i32 0\n");
    out.push_str("}\n\n");
    out
}

fn gen_cfunc(name: &str, params: &[Param], returns: &Option<Typ>, code: &str) -> String {
    let ret_type = match returns {
        Some(typ) => match typ { Typ::TFloat64 => "double", Typ::TFloat32 => "float", Typ::TBool | Typ::TChar => "i8", _ => "i64" },
        None => "i64",
    };
    let param_strs: Vec<String> = params.iter().map(|p| {
        match p { Param::PRef { typ, .. } | Param::POwn { typ, .. } => {
            match typ { Typ::TFloat64 => "double".to_string(), Typ::TFloat32 => "float".to_string(), Typ::TBool | Typ::TChar => "i8".to_string(), _ => "i64".to_string() }
        }}
    }).collect();
    format!("define {} @{}({}) {{\n; raw code:\n; {}\n  ret {} 0\n}}\n\n", ret_type, name, param_strs.join(", "), code, ret_type)
}

fn gen_impl(_struct_name: &str, impls: &[ImplExpr]) -> String {
    let mut out = String::new();
    for impl_expr in impls { out.push_str(&format!("; impl operator for {:?} (user function {})\n", impl_expr.op, impl_expr.func)); }
    out
}

fn generate_with_scope(defs: &[Def], module: Option<&str>, struct_field_map: &HashMap<String, HashMap<String, usize>>, func_sigs: &HashMap<String, crate::codegen::FuncSig>) -> (String, String, String, HashSet<String>) {
    let mut struct_defs = String::new();
    let mut defs_str = String::new();
    let mut main_functions = String::new();
    let mut defined = HashSet::new();

    for def in defs {
        match def {
            Def::DFunc { name, type_params, params, returns, body, .. } if name == "main" => main_functions.push_str(&gen_main_func(body, struct_field_map, func_sigs)),
            Def::DCFuncUnsafe { name, params, returns, code, .. } => defs_str.push_str(&gen_cfunc(name, params, returns, code)),
            Def::DFunc { name, type_params, params, returns, body, is_async, .. } => {
                let global_name = make_global_name(module, name.as_str());
                defined.insert(global_name);
                defs_str.push_str(&gen_func_def(name, type_params, params, returns, body, module, struct_field_map, func_sigs, *is_async));
            }
            Def::DImpl { struct_name, impls, .. } => defs_str.push_str(&gen_impl(struct_name, impls)),
            Def::DModule { name, .. } => {
                let inner = generate_with_scope(&defs[1..], Some(name.as_str()), struct_field_map, func_sigs);
                struct_defs.push_str(&inner.0); defs_str.push_str(&inner.1); main_functions.push_str(&inner.2);
                defined.extend(inner.3);
                break;
            }
            _ => {}
        }
    }
    (struct_defs, defs_str, main_functions, defined)
}

fn generate_test(defs: &[Def]) -> String {
    let mut test_ir = String::new();
    for def in defs { if let Def::DTest { name, .. } = def { test_ir.push_str(&format!("define i64 @{}() {{\nentry:\n  ret i64 0\n}}\n\n", name)); } }
    test_ir
}

fn generate_bridge(_defs: &[Def]) -> String {
    let mut bridge = String::new();
    bridge.push_str("#include <string>\n#include <vector>\n#include <thread>\n#include <mutex>\n#include <condition_variable>\n#include <cstdint>\n#include <cstdlib>\n#include <cstdio>\n#include <mvp_builtin.h>\n\nextern \"C\" {\n");
    bridge.push_str("void miva_print(void* s) { auto& str = *(std::string*)s; mvp_print(str); }\n");
    bridge.push_str("void miva_println(void* s) { auto& str = *(std::string*)s; mvp_println(str); }\n");
    bridge.push_str("void miva_prints(void* s) { auto& str = *(std::string*)s; mvp_prints(str); }\n");
    bridge.push_str("void miva_printlns(void* s) { auto& str = *(std::string*)s; mvp_printlns(str); }\n");
    bridge.push_str("void miva_error(void* s) { auto& str = *(std::string*)s; mvp_error(str); }\n");
    bridge.push_str("void miva_errors(void* s) { auto& str = *(std::string*)s; mvp_errors(str); }\n");
    bridge.push_str("void miva_errorln(void* s) { auto& str = *(std::string*)s; mvp_errorln(str); }\n");
    bridge.push_str("void miva_errorlns(void* s) { auto& str = *(std::string*)s; mvp_errorlns(str); }\n");
    bridge.push_str("void miva_exit(int64_t c) { mvp_exit(c); }\n");
    bridge.push_str("void miva_abort() { mvp_abort(); }\n");
    bridge.push_str("void miva_panic(void* s) { auto& str = *(std::string*)s; mvp_panic(str); }\n");
    bridge.push_str("void* miva_string_concat(void* a, void* b) { auto r = mvp_string_concat(*(std::string*)a, *(std::string*)b); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_string_parse(void* s) { return mvp_string_parse(*(std::string*)s); }\n");
    bridge.push_str("int64_t miva_string_length(void* s) { return mvp_string_length(*(std::string*)s); }\n");
    bridge.push_str("void* miva_string_make(void* init, int64_t size) { return new std::string(mvp_string_make(*(std::string*)init, size)); }\n");
    bridge.push_str("void* miva_string_from_int(int64_t v) { return new std::string(mvp_to_string(v)); }\n");
    bridge.push_str("void* miva_string_from_float(double v) { return new std::string(mvp_to_string(v)); }\n");
    bridge.push_str("void* miva_string_from_bool(int8_t v) { return new std::string(mvp_to_string(v)); }\n");
    bridge.push_str("void* miva_string_from_str(const char* s) { return new std::string(s); }\n");
    bridge.push_str("void miva_box_new_int(void** out, int64_t v) { *out = new mvp_builtin_box<mvp_builtin_int>(v); }\n");
    bridge.push_str("void miva_box_new_float(void** out, double v) { *out = new mvp_builtin_box<mvp_builtin_float>(v); }\n");
    bridge.push_str("void miva_box_new_bool(void** out, int8_t v) { *out = new mvp_builtin_box<mvp_builtin_boolean>(v); }\n");
    bridge.push_str("void miva_box_new_byte(void** out, int8_t v) { *out = new mvp_builtin_box<mvp_builtin_byte>(v); }\n");
    bridge.push_str("void miva_box_new_string(void** out, void* s) { *out = s; }\n");
    bridge.push_str("int64_t miva_box_deref_int(void* b) { return **(mvp_builtin_box<mvp_builtin_int>*)b; }\n");
    bridge.push_str("double miva_box_deref_float(void* b) { return **(mvp_builtin_box<mvp_builtin_float>*)b; }\n");
    bridge.push_str("int8_t miva_box_deref_bool(void* b) { return **(mvp_builtin_box<mvp_builtin_boolean>*)b; }\n");
    bridge.push_str("int8_t miva_box_deref_byte(void* b) { return **(mvp_builtin_box<mvp_builtin_byte>*)b; }\n");
    bridge.push_str("void miva_box_deref_string(void* out, void* b) { *(std::string*)out = **(mvp_builtin_box<mvp_builtin_string>*)b; }\n");
    bridge.push_str("void miva_range(void** out, int64_t start, int64_t end) { *out = new std::vector<mvp_builtin_int>(mvp_range(start,end)); }\n");
    bridge.push_str("void* miva_alloc(int64_t s) { return mvp_alloc(s); }\n");
    bridge.push_str("void* miva_realloc(void* p, int64_t s) { return mvp_realloc(p,s); }\n");
    bridge.push_str("void miva_free(void* p) { mvp_free(p); }\n");
    bridge.push_str("void* miva_ptr_offset(void* p, int64_t n) { return mvp_ptr_offset(p, n); }\n");
    bridge.push_str("int64_t miva_json_parse(void* s) { auto& str = *(std::string*)s; return (int64_t)(intptr_t)mvp_json_parse(str); }\n");
    bridge.push_str("int64_t miva_json_kind(int64_t v) { return mvp_json_kind((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_json_bool(int64_t v) { return mvp_json_bool((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_json_number(int64_t v) { double d = mvp_json_number((void*)(intptr_t)v); int64_t r; memcpy(&r, &d, 8); return r; }\n");
    bridge.push_str("void* miva_json_string(int64_t v) { auto r = mvp_json_string((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_json_array_len(int64_t v) { return mvp_json_array_len((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_json_array_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_json_array_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("int64_t miva_json_object_len(int64_t v) { return mvp_json_object_len((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_json_object_key(int64_t v, int64_t i) { auto r = mvp_json_object_key((void*)(intptr_t)v, i); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_json_object_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_json_object_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("int64_t miva_json_object_find(int64_t v, void* key) { auto& k = *(std::string*)key; return (int64_t)(intptr_t)mvp_json_object_find((void*)(intptr_t)v, k); }\n");
    bridge.push_str("void miva_json_free(int64_t v) { mvp_json_free((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_json_stringify(int64_t v) { auto r = mvp_json_stringify((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_xml_parse(void* s) { auto& str = *(std::string*)s; return (int64_t)(intptr_t)mvp_xml_parse(str); }\n");
    bridge.push_str("int64_t miva_xml_kind(int64_t v) { return mvp_xml_kind((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_xml_tag(int64_t v) { auto r = mvp_xml_tag((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_xml_attr_count(int64_t v) { return mvp_xml_attr_count((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_xml_attr_name(int64_t v, int64_t i) { auto r = mvp_xml_attr_name((void*)(intptr_t)v, i); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_attr_value(int64_t v, int64_t i) { auto r = mvp_xml_attr_value((void*)(intptr_t)v, i); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_attr_find(int64_t v, void* key) { auto& k = *(std::string*)key; auto r = mvp_xml_attr_find((void*)(intptr_t)v, k); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_xml_child_count(int64_t v) { return mvp_xml_child_count((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_xml_child_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_xml_child_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("void* miva_xml_text(int64_t v) { auto r = mvp_xml_text((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_comment(int64_t v) { auto r = mvp_xml_comment((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_cdata(int64_t v) { auto r = mvp_xml_cdata((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_pi_target(int64_t v) { auto r = mvp_xml_pi_target((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_pi_data(int64_t v) { auto r = mvp_xml_pi_data((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void* miva_xml_stringify(int64_t v) { auto r = mvp_xml_stringify((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void miva_xml_free(int64_t v) { mvp_xml_free((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_toml_parse(void* s) { auto& str = *(std::string*)s; return (int64_t)(intptr_t)mvp_toml_parse(str); }\n");
    bridge.push_str("int64_t miva_toml_kind(int64_t v) { return mvp_toml_kind((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_toml_bool(int64_t v) { return mvp_toml_bool((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_toml_number(int64_t v) { double d = mvp_toml_number((void*)(intptr_t)v); int64_t r; memcpy(&r, &d, 8); return r; }\n");
    bridge.push_str("void* miva_toml_string(int64_t v) { auto r = mvp_toml_string((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_toml_array_len(int64_t v) { return mvp_toml_array_len((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_toml_array_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_toml_array_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("int64_t miva_toml_object_len(int64_t v) { return mvp_toml_object_len((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_toml_object_key(int64_t v, int64_t i) { auto r = mvp_toml_object_key((void*)(intptr_t)v, i); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_toml_object_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_toml_object_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("int64_t miva_toml_object_find(int64_t v, void* key) { auto& k = *(std::string*)key; return (int64_t)(intptr_t)mvp_toml_object_find((void*)(intptr_t)v, k); }\n");
    bridge.push_str("void miva_toml_free(int64_t v) { mvp_toml_free((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_toml_stringify(int64_t v) { auto r = mvp_toml_stringify((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_yaml_parse(void* s) { auto& str = *(std::string*)s; return (int64_t)(intptr_t)mvp_yaml_parse(str); }\n");
    bridge.push_str("int64_t miva_yaml_kind(int64_t v) { return mvp_yaml_kind((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_yaml_bool(int64_t v) { return mvp_yaml_bool((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_yaml_number(int64_t v) { double d = mvp_yaml_number((void*)(intptr_t)v); int64_t r; memcpy(&r, &d, 8); return r; }\n");
    bridge.push_str("void* miva_yaml_string(int64_t v) { auto r = mvp_yaml_string((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_yaml_array_len(int64_t v) { return mvp_yaml_array_len((void*)(intptr_t)v); }\n");
    bridge.push_str("int64_t miva_yaml_array_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_yaml_array_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("int64_t miva_yaml_object_len(int64_t v) { return mvp_yaml_object_len((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_yaml_object_key(int64_t v, int64_t i) { auto r = mvp_yaml_object_key((void*)(intptr_t)v, i); return new std::string(std::move(r)); }\n");
    bridge.push_str("int64_t miva_yaml_object_get(int64_t v, int64_t i) { return (int64_t)(intptr_t)mvp_yaml_object_get((void*)(intptr_t)v, i); }\n");
    bridge.push_str("int64_t miva_yaml_object_find(int64_t v, void* key) { auto& k = *(std::string*)key; return (int64_t)(intptr_t)mvp_yaml_object_find((void*)(intptr_t)v, k); }\n");
    bridge.push_str("void miva_yaml_free(int64_t v) { mvp_yaml_free((void*)(intptr_t)v); }\n");
    bridge.push_str("void* miva_yaml_stringify(int64_t v) { auto r = mvp_yaml_stringify((void*)(intptr_t)v); return new std::string(std::move(r)); }\n");
    bridge.push_str("void miva_ptr_set_i64(void* p, int64_t v) { mvp_builtin_ptrset((mvp_builtin_int*)p, v); }\n");
    bridge.push_str("void miva_ptr_set_double(void* p, double v) { mvp_builtin_ptrset((mvp_builtin_float*)p, v); }\n");
    bridge.push_str("void miva_ptr_set_i8(void* p, int8_t v) { mvp_builtin_ptrset((mvp_builtin_byte*)p, v); }\n");
    bridge.push_str("void miva_ptr_set_ptr(void* p, void* v) { mvp_builtin_ptrset((mvp_builtin_ptrany*)p, v); }\n");
    bridge.push_str("struct mvp_async_task {\n  std::mutex mutex;\n  std::condition_variable cv;\n  bool done = false;\n  int64_t result = 0;\n  std::thread thread;\n};\n");
    bridge.push_str("int64_t miva_async_spawn(int64_t (*fn)(int64_t), int64_t arg_struct_ptr) {\n  auto* task = new mvp_async_task();\n  task->thread = std::thread([task, fn, arg_struct_ptr]() {\n    int64_t r = fn(arg_struct_ptr);\n    free((void*)(intptr_t)arg_struct_ptr);\n    {\n      std::lock_guard<std::mutex> lk(task->mutex);\n      task->result = r;\n      task->done = true;\n    }\n    task->cv.notify_one();\n  });\n  return (int64_t)(intptr_t)task;\n}\n");
    bridge.push_str("int64_t miva_async_await(int64_t handle) {\n  auto* task = (mvp_async_task*)(intptr_t)handle;\n  {\n    std::unique_lock<std::mutex> lk(task->mutex);\n    task->cv.wait(lk, [&] { return task->done; });\n  }\n  int64_t r = task->result;\n  task->thread.join();\n  delete task;\n  return r;\n}\n");
    bridge.push_str("}\n");
    bridge
}

pub fn build_ir(defs: &[Def], func_sigs: &HashMap<String, crate::codegen::FuncSig>) -> crate::codegen::GeneratedOutput {
    *EXTERN_DECLS.lock().unwrap() = Some(HashSet::new());
    let struct_types = collect_struct_types(defs);
    let struct_field_map = build_struct_field_map(defs);
    let (struct_defs, defs_str, main_functions, defined) = generate_with_scope(defs, None, &struct_field_map, func_sigs);

    let mut program = String::new();
    program.push_str("; ModuleID = 'miva_output'\n");
    program.push_str(&format!("target triple = \"{}\"\n\n", TARGET_TRIPLE));
    STR_CONST_COUNTER.store(0, Ordering::Relaxed);
    program.push_str("%mvp_builtin_string = type opaque\n");
    program.push_str("%mvp_builtin_box = type opaque\n\n");

    for st in &struct_types { program.push_str(&st); program.push_str("\n"); }

    program.push_str(&runtime_declarations());
    if let Ok(guard) = EXTERN_DECLS.lock() {
        if let Some(decls) = guard.as_ref() {
            for name in decls.iter() {
                if !defined.contains(name) {
                    program.push_str(&format!("declare i64 @{}()\n", name));
                }
            }
        }
    }

    program.push_str(&struct_defs);
    program.push_str(&defs_str);
    program.push_str(&main_functions);

    let test_ir = generate_test(defs);
    let bridge = generate_bridge(defs);

    crate::codegen::GeneratedOutput { program: program.into_bytes(), header: bridge, test: test_ir, extension: "ll" }
}
