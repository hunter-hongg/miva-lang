use std::collections::{HashMap, HashSet};

use crate::ast::*;

use miva_vm::vm::{MvmFunction, MvmProgram};
use miva_vm::Opcode as MvmOp;

/// Label management for jump resolution.
struct Label {
    /// Absolute code position where this label is defined (None if not yet defined).
    pos: Option<usize>,
    /// List of (code_position, is_if_not) pairs that need patching.
    pending: Vec<(usize, bool)>,
}

/// A user `unsafe fn` whose body is raw C, implemented in the project's
/// single `libhost.so`. Emitted as a `CallHost` at call sites and collected
/// so the build step can generate the matching C shim.
pub struct HostDef {
    pub name: String,
    pub arity: u32,
    pub returns: Option<Typ>,
    pub code: String,
}

/// Miva VM bytecode code generator.
pub struct MvmCodegen {
    // --- String pool ---
    string_pool: Vec<String>,
    string_indices: HashMap<String, u32>,

    // --- Function table ---
    functions: Vec<MvmFunction>,
    func_indices: HashMap<String, usize>,
    builtin_indices: HashMap<String, u8>,
    /// Names of user `unsafe fn`s implemented as host functions in libhost.so.
    host_funcs: HashSet<String>,
    /// For each function, the list of PRef parameter names.
    func_ref_params: HashMap<String, Vec<String>>,
    /// Collected user `unsafe fn` definitions (raw C) for libhost.so generation.
    host_defs: Vec<HostDef>,
    /// For each function that is `void` AND has at least one PRef parameter,
    /// the list of its PRef parameter names. Such functions return the mutated
    /// struct to the caller, which then stores it back into the arg's local.
    /// Non-void ref-param functions (e.g. `get`, `len`, `pop`) return their real
    /// result and must NOT trigger the caller-side store-back.
    void_ref_params: HashMap<String, Vec<String>>,

    // --- Struct field maps ---
    struct_field_indices: HashMap<String, Vec<(String, usize)>>, // struct name -> [(field_name, index)]

    // --- Impl table (operator overloading) ---
    impl_map: HashMap<String, HashMap<String, String>>, // struct_name -> { op_name -> func_name }

    // --- Current function compilation state ---
    code: Vec<u8>,
    /// Opcode of the most recently emitted instruction (None before any emit).
    /// Used to decide whether a trailing `Ret`/`RetVal` already terminates the
    /// function body, so we don't double-return or misread a multi-byte
    /// instruction's operand byte as a terminal opcode.
    last_emitted: Option<MvmOp>,
    locals_count: u32,
    local_indices: HashMap<String, Vec<u32>>, // name -> stack of indices (handles shadowing)
    scope_stack: Vec<u32>,
    param_types: HashMap<String, Typ>, // parameter name -> type
    /// Set of local indices that hold reference pointers (PRef params).
    ptr_params: HashSet<u32>,

    // --- Label management ---
    labels: HashMap<u32, Label>,
    next_label: u32,

    // --- Current function info ---
    current_func_name: String,
}

impl MvmCodegen {
    pub fn new() -> Self {
        let mut builtin_indices = HashMap::new();
        let builtins = [
            ("print", 0u8), ("prints", 1), ("println", 2), ("printlns", 3),
            ("error", 4), ("errors", 5), ("errorln", 6), ("errorlns", 7),
            ("exit", 8), ("abort", 9), ("panic", 10),
            ("string_concat", 11), ("string_length", 12), ("string_parse", 13),
            ("string_make", 14), ("string_from", 15), ("string_get", 16),
            ("box_new", 17), ("box_deref", 18), ("box_set", 19),
            ("range", 20), ("to_string", 21), ("read_int", 22), ("read_line", 23),
            ("json_parse", 24), ("json_kind", 25), ("json_bool", 26),
            ("json_number", 27), ("json_string", 28), ("json_array_len", 29),
            ("json_array_get", 30), ("json_object_len", 31), ("json_object_key", 32),
            ("json_object_get", 33), ("json_object_find", 34), ("json_free", 35),
            ("json_stringify", 36),
            ("xml_parse", 37), ("xml_kind", 38), ("xml_tag", 39),
            ("xml_attr_count", 40), ("xml_attr_name", 41), ("xml_attr_value", 42),
            ("xml_attr_find", 43), ("xml_child_count", 44), ("xml_child_get", 45),
            ("xml_text", 46), ("xml_comment", 47), ("xml_cdata", 48),
            ("xml_pi_target", 49), ("xml_pi_data", 50),             ("xml_stringify", 51),
            ("xml_free", 52),
            ("toml_parse", 53), ("toml_kind", 54), ("toml_bool", 55),
            ("toml_number", 56), ("toml_string", 57), ("toml_array_len", 58),
            ("toml_array_get", 59), ("toml_object_len", 60), ("toml_object_key", 61),
            ("toml_object_get", 62), ("toml_object_find", 63), ("toml_free", 64),
            ("toml_stringify", 65),
            ("yaml_parse", 66), ("yaml_kind", 67), ("yaml_bool", 68),
            ("yaml_number", 69), ("yaml_string", 70), ("yaml_array_len", 71),
            ("yaml_array_get", 72), ("yaml_object_len", 73), ("yaml_object_key", 74),
            ("yaml_object_get", 75), ("yaml_object_find", 76), ("yaml_free", 77),
            ("yaml_stringify", 78),
            ("ptr_alloc", 79), ("ptr_free", 80), ("ptr_realloc", 81),
            ("ptr_offset", 82), ("ptr_set", 83), ("ptr_ref", 84),
        ];
        for (name, idx) in builtins {
            builtin_indices.insert(name.to_string(), idx);
        }
        MvmCodegen {
            string_pool: Vec::new(),
            string_indices: HashMap::new(),
            functions: Vec::new(),
            func_indices: HashMap::new(),
            builtin_indices,
            host_funcs: HashSet::new(),
            func_ref_params: HashMap::new(),
            host_defs: Vec::new(),
            void_ref_params: HashMap::new(),
            struct_field_indices: HashMap::new(),
            impl_map: HashMap::new(),
            code: Vec::new(),
            last_emitted: None,
            locals_count: 0,
            local_indices: HashMap::new(),
            scope_stack: Vec::new(),
            param_types: HashMap::new(),
            ptr_params: HashSet::new(),
            labels: HashMap::new(),
            next_label: 0,
            current_func_name: String::new(),
        }
    }
    fn resolve_string(&mut self, s: &str) -> u32 {
        if let Some(&idx) = self.string_indices.get(s) {
            return idx;
        }
        let idx = self.string_pool.len() as u32;
        self.string_pool.push(s.to_string());
        self.string_indices.insert(s.to_string(), idx);
        idx
    }

    // --- Bytecode emitter helpers ---

    fn emit_u8(&mut self, b: u8) {
        self.code.push(b);
    }

    fn emit_u32(&mut self, v: u32) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    fn emit_i32(&mut self, v: i32) {
        let before = self.code.len();
        self.code.extend_from_slice(&v.to_le_bytes());
        let after = self.code.len();
        if after - before != 4 {
        }
    }

    fn emit_i64(&mut self, v: i64) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    fn emit_f64(&mut self, v: f64) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    // --- Label management ---
    fn emit_op(&mut self, op: MvmOp) {
        self.code.push(op as u8);
        self.last_emitted = Some(op);
    }

    fn new_label(&mut self) -> u32 {
        let id = self.next_label;
        self.next_label += 1;
        self.labels.insert(id, Label { pos: None, pending: Vec::new() });
        id
    }

    fn define_label(&mut self, id: u32) {
        let pos = self.code.len();
        let label = self.labels.get_mut(&id).expect("label not found");
        label.pos = Some(pos);
        // Resolve pending jumps targeting this label
        let pending = std::mem::take(&mut label.pending);
        for (jump_pos, _is_if_not) in pending {
            let offset = pos as i32 - jump_pos as i32 - 4; // -4 for the offset field itself
            let offset_bytes = offset.to_le_bytes();
            self.code[jump_pos..jump_pos + 4].copy_from_slice(&offset_bytes);
        }
    }

    fn emit_jmp(&mut self, target_label: u32) {
        let pos = self.code.len();
        self.emit_op(MvmOp::Jmp);
        self.emit_i32(0); // placeholder
        let label = self.labels.get_mut(&target_label).unwrap();
        if let Some(target_pos) = label.pos {
            // offset relative to end of instruction (pos + 5 = next instruction position)
            let offset = target_pos as i32 - pos as i32 - 5;
            let len = self.code.len();
            self.code[len - 4..len].copy_from_slice(&offset.to_le_bytes());
        } else {
            label.pending.push((pos + 1, false)); // +1 to skip opcode byte
        }
    }

    fn emit_jmp_if_true(&mut self, target_label: u32) {
        let pos = self.code.len();
        self.emit_op(MvmOp::JmpIf);
        self.emit_i32(0); // placeholder
        let label = self.labels.get_mut(&target_label).unwrap();
        if let Some(target_pos) = label.pos {
            let offset = target_pos as i32 - pos as i32 - 5;
            let len = self.code.len();
            self.code[len - 4..len].copy_from_slice(&offset.to_le_bytes());
        } else {
            label.pending.push((pos + 1, true));
        }
    }

    fn emit_jmp_if_false(&mut self, target_label: u32) {
        let pos = self.code.len();
        self.emit_op(MvmOp::JmpIfNot);
        self.emit_i32(0); // placeholder
        let label = self.labels.get_mut(&target_label).unwrap();
        if let Some(target_pos) = label.pos {
            let offset = target_pos as i32 - pos as i32 - 5;
            let len = self.code.len();
            self.code[len - 4..len].copy_from_slice(&offset.to_le_bytes());
        } else {
            label.pending.push((pos + 1, false));
        }
    }

    // --- Variable scope management ---

    fn push_scope(&mut self) {
        self.scope_stack.push(self.locals_count);
    }

    fn pop_scope(&mut self) {
        let _old_count = self.scope_stack.pop().expect("scope stack underflow");
        // Don't reset locals_count — variables from nested scopes stay
        // in the function's frame (they are just unused slots after scope exit).
        // Only clean up the name-to-index mapping so the names are no longer
        // accessible. The slots remain allocated for simplicity.
        self.local_indices.retain(|_, indices| {
            indices.retain(|&idx| idx < _old_count);
            !indices.is_empty()
        });
    }

    fn declare_local(&mut self, name: &str) -> u32 {
        let idx = self.locals_count;
        self.locals_count += 1;
        self.local_indices.entry(name.to_string())
            .or_default()
            .push(idx as u32);
        idx as u32
    }

    fn resolve_local(&self, name: &str) -> Option<u32> {
        self.local_indices.get(name)
            .and_then(|indices| indices.last().copied())
    }

    // --- Struct field info (populated during initialization) ---

    fn collect_struct_info(&mut self, defs: &[Def]) {
        for def in defs {
            match def {
                Def::DStruct { name, fields, .. } => {
                    let indexed: Vec<(String, usize)> = fields.iter()
                        .enumerate()
                        .map(|(i, f)| (f.name.clone(), i))
                        .collect();
                    self.struct_field_indices.insert(name.clone(), indexed);
                }
                Def::DEnum { name, variants, .. } => {
                    let indexed: Vec<(String, usize)> = variants.iter()
                        .enumerate()
                        .map(|(i, v)| (v.name.clone(), i))
                        .collect();
                    self.struct_field_indices.insert(name.clone(), indexed);
                }
                Def::DImpl { struct_name, impls, .. } => {
                    let ops = self.impl_map.entry(struct_name.clone()).or_default();
                    for impl_expr in impls {
                        let op_name = match impl_expr.op {
                            ImplOp::ImAdd => "add",
                            ImplOp::ImSub => "sub",
                            ImplOp::ImMul => "mul",
                            ImplOp::ImEq => "eq",
                            ImplOp::ImNeq => "neq",
                        };
                        ops.insert(op_name.to_string(), impl_expr.func.clone());
                    }
                }
                _ => {}
            }
        }
    }

    // --- Main compilation entry ---
    pub fn build_ir(defs: &[Def]) -> (MvmProgram, Vec<HostDef>) {
        let mut cg = MvmCodegen::new();
        cg.collect_struct_info(defs);

        // Pass 1: collect function names and signatures
        // Store both the bare name (for lookup_name compatibility) and the
        // fully qualified name (to disambiguate functions with the same name
        // from different modules, e.g. `free` in both vec.miva and mem.miva).
        for def in defs {
            match def {
                Def::DFunc { name, params, is_async, returns, .. } => {
                    if !cg.func_indices.contains_key(name) {
                        let idx = cg.functions.len();
                        cg.func_indices.insert(name.clone(), idx);
                        // Placeholder function (will be filled in pass 2)
                        cg.functions.push(MvmFunction {
                            name_idx: 0,
                            arity: params.len() as u32,
                            locals: 0,
                            is_async: *is_async,
                            code: Vec::new(),
                        });
                    }
                    // Collect PRef parameter names for reference-parameter
                    // analysis (used by SExpr caller-store-back logic).
                    let ref_param_names: Vec<String> = params
                        .iter()
                        .filter_map(|p| {
                            if let Param::PRef { name, .. } = p {
                                Some(name.clone())
                            } else {
                                None
                            }
                        })
                        .collect();
                    cg.func_ref_params.insert(name.clone(), ref_param_names.clone());
                    if returns.is_none() && !ref_param_names.is_empty() {
                        cg.void_ref_params.insert(name.clone(), ref_param_names);
                    }
                }
                Def::DTest { name, .. } => {
                    if !cg.func_indices.contains_key(name) {
                        let idx = cg.functions.len();
                        cg.func_indices.insert(name.clone(), idx);
                        cg.functions.push(MvmFunction {
                            name_idx: 0,
                            arity: 0,
                            locals: 0,
                            is_async: false,
                            code: Vec::new(),
                        });
                    }
                }
                Def::DCFuncUnsafe { name, params, returns, code, .. } => {
                    if !cg.host_funcs.contains(name) {
                        cg.host_funcs.insert(name.clone());
                        cg.host_defs.push(HostDef {
                            name: name.clone(),
                            arity: params.len() as u32,
                            returns: returns.clone(),
                            code: code.clone(),
                        });
                    }
                }
                _ => {}
            }
        }

        // Pass 2: compile each function body
        for def in defs {
            match def {
                Def::DFunc { name, params, returns, body, .. } => {
                    let func_idx = cg.func_indices[name];
                    cg.current_func_name = name.clone();
                    cg.compile_function(func_idx, params, body, false, returns);
                }
                Def::DTest { name, body, .. } => {
                    let func_idx = cg.func_indices[name];
                    cg.current_func_name = name.clone();
                    cg.compile_function(func_idx, &[], body, true, &None);
                }
                _ => {}
            }
        }

        // Fill in name indices (now that all strings are collected)
        let func_names: Vec<(String, usize)> = cg.func_indices.iter()
            .map(|(name, &idx)| (name.clone(), idx))
            .collect();
        for (func_name, idx) in &func_names {
            let name_idx = cg.resolve_string(func_name);
            cg.functions[*idx].name_idx = name_idx;
        }

        let program = MvmProgram {
            strings: cg.string_pool,
            functions: cg.functions,
        };

        (program, cg.host_defs)
    }

    fn compile_function(&mut self, func_idx: usize, params: &[Param], body: &Expr, _is_test: bool, returns: &Option<Typ>) {
        // Reset state for new function
        self.code = Vec::new();
        self.locals_count = 0;
        self.local_indices.clear();
        self.scope_stack.clear();
        self.labels.clear();
        self.next_label = 0;
        self.param_types.clear();

        // Declare parameters as locals and store their types
        for param in params {
            match param {
                Param::PRef { name, typ, .. } | Param::POwn { name, typ, .. } => {
                    self.declare_local(name);
                    self.param_types.insert(name.clone(), typ.clone());
                }
            }
        }

        // Compile the function body
        // For non-void functions, handle implicit return from EBlock last expression.
        if returns.is_some() {
            if let Expr::EBlock { stmts, result, .. } = body {
                if result.is_none() && !stmts.is_empty() {
                    self.push_scope();
                    // Compile all stmts except the last one normally.
                    for stmt in &stmts[..stmts.len() - 1] {
                        self.compile_stmt(stmt);
                    }
                    // Compile the last stmt as an expression (no trailing Drop).
                    if let Stmt::SExpr { expr, .. } = &stmts[stmts.len() - 1] {
                        self.compile_expr(expr);
                    } else {
                        self.compile_stmt(&stmts[stmts.len() - 1]);
                    }
                    self.pop_scope();
                    if !matches!(self.last_emitted, Some(MvmOp::Ret | MvmOp::RetVal)) {
                        self.emit_op(MvmOp::RetVal);
                    }
                    // IMPORTANT: update function table BEFORE returning, otherwise
                    // the bytecode we just generated is lost (the normal path's
                    // function table update at the end of this method is skipped).
                    let arity = params.len() as u32;
                    self.functions[func_idx].arity = arity;
                    self.functions[func_idx].locals = self.locals_count;
                    self.functions[func_idx].code = std::mem::take(&mut self.code);
                    return; // skip the `compile_expr(body)` and PushUnit below
                }
            }
        }
        self.push_scope();
        self.compile_expr(body);
        self.pop_scope();

        // If the function body didn't end with a return, add one. Use the
        // tracked last-emitted opcode so a multi-byte instruction's operand
        // byte (e.g. CallBuiltin's u8 index) is never mistaken for a terminal
        // Ret/RetVal opcode.
        let needs_ret = !self.code.is_empty()
            && !matches!(self.last_emitted, Some(MvmOp::Ret | MvmOp::RetVal));
        if needs_ret {
            // If this function has ref parameters AND returns void, return
            // the value of the first ref param so the caller can store the
            // modified struct back (ref params are passed by value in the
            // MVM backend; returning and re-storing is the simplest way to
            // propagate mutations).
            if returns.is_none() {
                let ref_param_names = self.func_ref_params.get(&self.current_func_name);
                if let Some(names) = ref_param_names {
                    if let Some(ref_name) = names.first() {
                        if let Some(idx) = self.resolve_local(ref_name) {
                            // Drop the body's result (typically Unit) and
                            // push the ref param value instead.
                            self.emit_op(MvmOp::Drop);
                            self.emit_op(MvmOp::LoadLocal);
                            self.emit_u32(idx);
                        }
                    }
                }
            }
            self.emit_op(MvmOp::RetVal);
        }
        self.last_emitted = None;

        // Update function in table
        let arity = params.len() as u32;
        self.functions[func_idx].arity = arity;
        self.functions[func_idx].locals = self.locals_count;
        self.functions[func_idx].code = std::mem::take(&mut self.code);
    }

    // --- Expression compilation ---

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::EInt { value, .. } => {
                self.emit_op(MvmOp::PushI64);
                self.emit_i64(*value);
            }
            Expr::EBool { value, .. } => {
                self.emit_op(MvmOp::PushBool);
                self.emit_u8(if *value { 1 } else { 0 });
            }
            Expr::EFloat { value, .. } => {
                // Default to float64 for constant floats
                self.emit_op(MvmOp::PushF64);
                self.emit_f64(*value);
            }
            Expr::EChar { value, .. } => {
                // Char values are stored as strings in Miva JSON but single char
                let c = value.chars().next().unwrap_or('\0') as u8;
                self.emit_op(MvmOp::PushChar);
                self.emit_u8(c);
            }
            Expr::EString { value, .. } => {
                let resolved = crate::codegen::resolve_c_escapes(value);
                let str_idx = self.resolve_string(&resolved);
                self.emit_op(MvmOp::PushString);
                self.emit_u32(str_idx);
            }
            Expr::EVar { name, .. } | Expr::EMove { name, .. } => {
                let idx = self.resolve_local(name)
                    .unwrap_or_else(|| panic!("Variable '{}' not found in scope", name));
                self.emit_op(MvmOp::LoadLocal);
                self.emit_u32(idx);
            }
            Expr::EClone { name, .. } => {
                let idx = self.resolve_local(name)
                    .unwrap_or_else(|| panic!("Variable '{}' not found in scope", name));
                self.emit_op(MvmOp::LoadLocal);
                self.emit_u32(idx);
                self.emit_op(MvmOp::Clone);
            }
            Expr::EStructLit { name, fields, .. } => {
                // Look up struct field definitions to determine field order
                let field_count = self.struct_field_indices.get(name).map(|fl| fl.len()).unwrap_or(0);
                if field_count > 0 {
                    // Emit field values in struct definition order
                    if let Some(field_list) = self.struct_field_indices.get(name) {
                        let mut field_values = vec![None; field_list.len()];
                        for field in fields {
                            if let Some(&(_, idx)) = field_list.iter().find(|(fname, _)| *fname == field.name) {
                                field_values[idx] = Some(&field.value);
                            }
                        }
                        for val in &field_values {
                            if let Some(v) = val {
                                self.compile_expr(v);
                            }
                        }
                    }
                    self.emit_op(MvmOp::StructNew);
                    self.emit_u32(field_count as u32);
                } else {
                    // Unknown struct type; just push unit
                    self.emit_op(MvmOp::PushUnit);
                }
            }
            Expr::EFieldAccess { expr: obj, field, .. } => {
                if field.chars().all(|c| c.is_ascii_digit()) {
                    self.compile_expr(obj);
                    self.emit_op(MvmOp::EnumGet);
                    self.emit_u32(field.parse::<u32>().unwrap());
                } else {
                    self.compile_expr(obj);
                    if let Some(field_list) = self.find_field_list(obj) {
                        if let Some(idx) = field_list.iter().position(|(fname, _)| fname == field) {
                            self.emit_op(MvmOp::StructGet);
                            self.emit_u32(idx as u32);
                        } else {
                            // Field not found; emit unit
                            self.emit_op(MvmOp::Drop);
                            self.emit_op(MvmOp::PushUnit);
                        }
                    } else {
                        self.emit_op(MvmOp::Drop);
                        self.emit_op(MvmOp::PushUnit);
                    }
                }
            }
            Expr::EBinOp { op, left, right, .. } => {
                // Check if this operation has an impl override
                if let Some(op_name) = self.find_impl_override(left, op) {
                    // Transform to function call
                    self.compile_expr(left);
                    self.compile_expr(right);
                    let func_idx = *self.func_indices.get(&op_name)
                        .expect(&format!("Impl function '{}' not found", op_name));
                    self.emit_op(MvmOp::Call);
                    self.emit_u32(func_idx as u32);
                } else {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.emit_binop(op);
                }
            }
            Expr::EIf { cond, then, else_, .. } => {
                let else_label = self.new_label();
                let end_label = self.new_label();

                self.compile_expr(cond);
                self.emit_jmp_if_false(else_label);
                let before_then = self.code.len();
                self.compile_expr(then);
                let after_then = self.code.len();
                self.emit_jmp(end_label);
                let after_jmp = self.code.len();
                self.define_label(else_label);
                let after_else_label = self.code.len();
                if let Some(else_expr) = else_ {
                    self.compile_expr(else_expr);
                } else {
                    self.emit_op(MvmOp::PushUnit);
                }
                self.define_label(end_label);
            }
            Expr::ECall { name, args, .. } => {
                // Handle module-qualified names: try bare name suffix
                let lookup_name = name.rsplit('.').next().unwrap_or(name);
                // Enum constructor / discriminant: `Name.Variant(args)` where
                // `Name` is a known enum. Compile inline to an EnumNew (or a
                // tag-only discriminant `Enum(tag, [])` for `when (Name.Variant)`).
                if let Some((enum_name, variant)) = name.split_once('.') {
                    if let Some(variant_list) = self.struct_field_indices.get(enum_name) {
                        if let Some(&(_, tag)) = variant_list.iter().find(|(vname, _)| *vname == variant) {
                            for arg in args {
                                self.compile_expr(arg);
                            }
                            self.emit_op(MvmOp::PushI64);
                            self.emit_i64(tag as i64);
                            self.emit_op(MvmOp::EnumNew);
                            self.emit_u32(args.len() as u32);
                            return;
                        }
                    }
                }
                // `await(f)` / `f.await()` unwraps a future (identity for non-futures)
                if lookup_name == "await" {
                    if let Some(arg) = args.first() {
                        self.compile_expr(arg);
                    } else {
                        self.emit_op(MvmOp::PushUnit);
                    }
                    self.emit_op(MvmOp::Await);
                    return;
                }
                // Check if builtin
                if let Some(&builtin_idx) = self.builtin_indices.get(lookup_name) {
                    // Compile args (they'll be on the stack for the builtin)
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.emit_op(MvmOp::CallBuiltin);
                    self.emit_u8(builtin_idx);
                } else if self.host_funcs.contains(lookup_name) {
                    // User `unsafe fn` with raw C -> host call into libhost.so.
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    let name_idx = self.resolve_string(lookup_name);
                    self.emit_op(MvmOp::CallHost);
                    self.emit_u32(name_idx);
                    self.emit_u8(args.len() as u8);
                } else if let Some(&func_idx) = self.func_indices.get(lookup_name) {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.emit_op(MvmOp::Call);
                    self.emit_u32(func_idx as u32);
                    // Async functions are spawned by the VM on call; the returned
                    // value is already a future[T], so no wrapping is needed.
                } else {
                    // Unknown function; compile args and push unit
                    for arg in args {
                        self.compile_expr(arg);
                        self.emit_op(MvmOp::Drop);
                    }
                    self.emit_op(MvmOp::PushUnit);
                }
            }
            Expr::ECast { expr: inner, to, .. } => {
                self.compile_expr(inner);
                self.emit_cast(to);
            }
            Expr::EBlock { stmts, result, .. } => {
                self.push_scope();
                for stmt in stmts {
                    self.compile_stmt(stmt);
                }
                if let Some(res) = result {
                    self.compile_expr(res);
                } else {
                    self.emit_op(MvmOp::PushUnit);
                }
                self.pop_scope();
            }
            Expr::EArrayLit { values, .. } => {
                // Push values first, then size, then ArrayNew
                for v in values {
                    self.compile_expr(v);
                }
                self.emit_op(MvmOp::PushI64);
                self.emit_i64(values.len() as i64);
                self.emit_op(MvmOp::ArrayNew);
            }
            Expr::EVoid { .. } => {
                self.emit_op(MvmOp::PushUnit);
            }
            Expr::EAddr { expr: inner, .. } => {
                // Take address of a variable
                match inner.as_ref() {
                    Expr::EVar { name, .. } | Expr::EMove { name, .. } => {
                        if let Some(idx) = self.resolve_local(name) {
                            self.emit_op(MvmOp::Addr);
                            self.emit_u32(idx);
                        } else {
                            self.emit_op(MvmOp::PushNull);
                        }
                    }
                    _ => {
                        self.emit_op(MvmOp::PushNull);
                    }
                }
            }
            Expr::EDeref { expr: inner, .. } => {
                self.compile_expr(inner);
                self.emit_op(MvmOp::PtrLoad);
            }
            Expr::EWhile { cond, body, .. } => {
                let cond_label = self.new_label();
                let end_label = self.new_label();

                self.define_label(cond_label);
                self.compile_expr(cond);
                self.emit_jmp_if_false(end_label);
                self.compile_expr(body);
                self.emit_op(MvmOp::Drop); // discard body result
                self.emit_jmp(cond_label);
                self.define_label(end_label);
                self.emit_op(MvmOp::PushUnit);
            }
            Expr::ELoop { body, .. } => {
                let loop_label = self.new_label();
                self.define_label(loop_label);
                self.compile_expr(body);
                self.emit_op(MvmOp::Drop);
                self.emit_jmp(loop_label);
            }
            Expr::EFor { var, range, body, .. } => {
                // Transform for-loop into a counter-based while loop.
                // We compile the range expression, then create a loop that:
                //  - initializes a counter to 0
                //  - compares it with the range end value at each iteration
                //  - increments the counter
                let check_label = self.new_label();
                let end_label = self.new_label();

                // Allocate a local for the loop counter (if not already)
                let counter_idx = self.declare_local("__for_counter__");
                // Allocate a local for the end value
                let end_idx = self.declare_local("__for_end__");

                // Get the range end value by calling range(n)
                // The range builtin pushes a Range(start, end, current) object
                self.compile_expr(range);
                // Stack: [Range(start, end, current)]
                // We need to extract the end value. For simplicity, call RangeNext
                // which pushes [updated_range, has_next, value]
                // But all we need is the end value. Let's use a simpler approach:
                // range(n) creates Range(0, n, 0) — the value 3 produces end=3
                // But we can't extract the end field from the range easily.
                // Alternative: call range(0, end) and iterate manually.
                // 
                // Actually, simplest approach: iterate using counter
                // The range expression on the stack is a Value::Range
                // We just need to loop n times where n is the only arg
                // 
                // For now, compile the range expression but use a manual counter
                // Drop the range (we don't need it anymore)
                self.emit_op(MvmOp::Drop);

                // Push end value (the range arg was already compiled, so it's
                // on the stack before the range builtin consumed it)
                // BUT: The range builtin consumed the arg! We need to compile
                // the range expression AGAIN to get the arg value.
                // 
                // Better approach: use the arg of range() directly
                // Recompile the range expression's arg
                match range.as_ref() {
                    Expr::ECall { args, .. } if !args.is_empty() => {
                        self.compile_expr(&args[0]);
                    }
                    _ => { self.emit_op(MvmOp::PushI64); self.emit_i64(0); }
                }
                // Stack: [end_value]
                self.emit_op(MvmOp::StoreLocal);
                self.emit_u32(end_idx);
                // Initialize counter = 0
                self.emit_op(MvmOp::PushI64);
                self.emit_i64(0);
                self.emit_op(MvmOp::StoreLocal);
                self.emit_u32(counter_idx);

                // === Loop check ===
                self.define_label(check_label);
                // Load counter
                self.emit_op(MvmOp::LoadLocal);
                self.emit_u32(counter_idx);
                // Load end
                self.emit_op(MvmOp::LoadLocal);
                self.emit_u32(end_idx);
                // Compare: counter < end ?
                self.emit_op(MvmOp::CmpLt);
                // If not less, exit loop
                self.emit_jmp_if_false(end_label);

                // Counter < end, so execute body with current counter value
                // Store counter value as the loop variable
                let var_idx = if let Some(idx) = self.resolve_local(var) {
                    idx
                } else {
                    self.declare_local(var)
                };
                self.emit_op(MvmOp::LoadLocal);
                self.emit_u32(counter_idx);
                self.emit_op(MvmOp::StoreLocal);
                self.emit_u32(var_idx);

                // Compile body
                self.compile_expr(body);
                self.emit_op(MvmOp::Drop); // discard body result

                // Increment counter
                self.emit_op(MvmOp::LoadLocal);
                self.emit_u32(counter_idx);
                self.emit_op(MvmOp::PushI64);
                self.emit_i64(1);
                self.emit_op(MvmOp::I64Add);
                self.emit_op(MvmOp::StoreLocal);
                self.emit_u32(counter_idx);

                // Jump back to check
                self.emit_jmp(check_label);
                self.define_label(end_label);
                self.emit_op(MvmOp::PushUnit);
            }
            Expr::EChoose { var, cases, otherwise, .. } => {
                let end_label = self.new_label();
                let case_labels: Vec<u32> = (0..cases.len()).map(|_| self.new_label()).collect();

                self.compile_expr(var);
                for (i, case) in cases.iter().enumerate() {
                    // Dup the var value for comparison
                    self.emit_op(MvmOp::Dup);
                    self.compile_expr(&case.when);
                    self.emit_op(MvmOp::CmpEq);
                    self.emit_jmp_if_false(case_labels[i]);
                    self.emit_op(MvmOp::Drop); // drop the dup'd var
                    self.compile_expr(&case.then);
                    self.emit_jmp(end_label);
                    self.define_label(case_labels[i]);
                }
                self.emit_op(MvmOp::Drop); // drop the var
                if let Some(other) = otherwise {
                    self.compile_expr(other);
                } else {
                    self.emit_op(MvmOp::PushUnit);
                }
                self.define_label(end_label);
            }
            Expr::EMacro { .. } => {} // Already expanded
            Expr::EMacroVar { .. } => { self.emit_op(MvmOp::PushUnit); }
            Expr::EMethodCall { .. } => {
                unreachable!("EMethodCall should be desugared by frontend");
            }
        }
    }

    // --- Statement compilation ---

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::SLet { name, expr, .. } | Stmt::SLetTyped { name, expr, .. } => {
                let idx = self.declare_local(name);
                self.compile_expr(expr);
                self.emit_op(MvmOp::StoreLocal);
                self.emit_u32(idx);
            }
            Stmt::SAssign { name, expr, .. } => {
                if let Some(idx) = self.resolve_local(name) {
                    self.compile_expr(expr);
                    self.emit_op(MvmOp::StoreLocal);
                    self.emit_u32(idx);
                } else {
                    // Variable not found; just evaluate expr and discard
                    self.compile_expr(expr);
                    self.emit_op(MvmOp::Drop);
                }
            }
            Stmt::SFieldAssign { target, field, expr, .. } => {
                // `target.field = expr` — MVM backend field write.
                // Compile the target struct, the new value, emit StructSet
                // with the field index, then store the modified struct back
                // into the target local slot.
                if let Expr::EVar { name, .. } | Expr::EMove { name, .. } = target.as_ref() {
                    if let Some(local_idx) = self.resolve_local(name) {
                        if let Some(field_list) = self.find_field_list(target) {
                            if let Some(idx) = field_list.iter().position(|(fname, _)| fname == field) {
                                self.compile_expr(target);
                                self.compile_expr(expr);
                                self.emit_op(MvmOp::StructSet);
                                self.emit_u32(idx as u32);
                                self.emit_op(MvmOp::StoreLocal);
                                self.emit_u32(local_idx);
                                return;
                            }
                        }
                    }
                }
                // Fallback for unhandled targets: evaluate both and discard.
                self.compile_expr(target);
                self.compile_expr(expr);
                self.emit_op(MvmOp::Drop);
                self.emit_op(MvmOp::Drop);
            }
            Stmt::SReturn { expr, .. } => {
                self.compile_expr(expr);
                // For void functions with ref parameters, the return should
                // carry the modified ref-param value back to the caller so
                // that the caller's SExpr store-back logic works correctly.
                // Drop the expression result and push the ref param instead.
                if self.current_func_name.is_empty() {
                    // Not inside a function (shouldn't happen)
                    self.emit_op(MvmOp::RetVal);
                } else {
                    // Only void functions with ref parameters return the mutated
                    // ref-param struct; non-void ref-param functions (e.g. `get`,
                    // `pop`) return their real expression result.
                    let is_void_ref = self.void_ref_params.contains_key(&self.current_func_name);
                    if is_void_ref {
                        let ref_param_names = self.func_ref_params.get(&self.current_func_name);
                        if let Some(names) = ref_param_names {
                            if let Some(ref_name) = names.first() {
                                if let Some(idx) = self.resolve_local(ref_name) {
                                    self.emit_op(MvmOp::Drop);
                                    self.emit_op(MvmOp::LoadLocal);
                                    self.emit_u32(idx);
                                }
                            }
                        }
                    }
                    self.emit_op(MvmOp::RetVal);
                }
            }
            Stmt::SExpr { expr, .. } => {
                // If the expression is a call to a function with ref
                // parameters, the called function now returns the modified
                // ref-param value (instead of Unit). Store it back into
                // the corresponding local slot.
                if let Expr::ECall { name, args, .. } = expr.as_ref() {
                    let lookup_name = name.rsplit('.').next().unwrap_or(name);
                    if let Some(ref_names) = self.void_ref_params.get(lookup_name) {
                        if let Some(first_ref) = ref_names.first() {
                            if let Some(pos) = ref_names.iter().position(|n| n == first_ref) {
                                if pos < args.len() {
                                    if let Expr::EVar { name, .. } | Expr::EMove { name, .. } = &args[pos] {
                                        if let Some(idx) = self.resolve_local(name) {
                                            self.compile_expr(expr);
                                            self.emit_op(MvmOp::StoreLocal);
                                            self.emit_u32(idx);
                                            return;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                self.compile_expr(expr);
                self.emit_op(MvmOp::Drop);
            }
            Stmt::SCIntro { .. } => {} // C code integration, skip
            Stmt::SEmpty { .. } => {}
        }
    }

    // --- Helper: emit binary operation ---

    fn emit_binop(&mut self, op: &BinOp) {
        match op {
            BinOp::Add => {
                // For simplicity, emit I64Add; the VM will handle type mismatches
                self.emit_op(MvmOp::I64Add);
            }
            BinOp::Sub => self.emit_op(MvmOp::I64Sub),
            BinOp::Mul => self.emit_op(MvmOp::I64Mul),
            BinOp::Eq => self.emit_op(MvmOp::CmpEq),
            BinOp::Neq => self.emit_op(MvmOp::CmpNeq),
            BinOp::Lt => self.emit_op(MvmOp::CmpLt),
            BinOp::Gt => self.emit_op(MvmOp::CmpGt),
            BinOp::Le => self.emit_op(MvmOp::CmpLe),
            BinOp::Ge => self.emit_op(MvmOp::CmpGe),
            BinOp::And => self.emit_op(MvmOp::I64And),
            BinOp::Or => self.emit_op(MvmOp::I64Or),
        }
    }

    // --- Helper: emit type cast ---

    fn emit_cast(&mut self, to: &Typ) {
        match to {
            Typ::TInt | Typ::TBool => {
                // no-op for common cases (int is the default)
            }
            Typ::TFloat64 => self.emit_op(MvmOp::I64ToF64),
            Typ::TFloat32 => self.emit_op(MvmOp::I64ToF32),
            Typ::TChar => self.emit_op(MvmOp::I64ToChar),
            _ => {}
        }
    }

    // --- Helper: find struct field list for an expression ---

    fn find_field_list(&self, expr: &Expr) -> Option<&Vec<(String, usize)>> {
        match expr {
            Expr::EVar { name, .. } | Expr::EMove { name, .. } => {
                if let Some(typ) = self.param_types.get(name) {
                    if let Typ::TStruct { name: struct_name, .. } = typ {
                        return self.struct_field_indices.get(struct_name);
                    }
                }
                None
            }
            Expr::EStructLit { name, .. } => {
                self.struct_field_indices.get(name)
            }
            _ => None,
        }
    }

    // --- Helper: find impl override for a binop ---

    fn find_impl_override(&self, left: &Expr, op: &BinOp) -> Option<String> {
        let op_name = match op {
            BinOp::Add => "add",
            BinOp::Sub => "sub",
            BinOp::Mul => "mul",
            BinOp::Eq => "eq",
            BinOp::Neq => "neq",
            BinOp::Lt => "lt",
            BinOp::Gt => "gt",
            BinOp::Le => "le",
            BinOp::Ge => "ge",
            BinOp::And => "and",
            BinOp::Or => "or",
        };

        // Try to find any impl that matches this operator
        for (_struct_name, impls) in &self.impl_map {
            if let Some(func_name) = impls.get(op_name) {
                return Some(func_name.clone());
            }
        }
        None
    }
}

/// Build MVM bytecode for the given AST definitions.
pub fn build_ir(defs: &[Def]) -> (MvmProgram, Vec<HostDef>) {
    MvmCodegen::build_ir(defs)
}
