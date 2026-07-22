use std::collections::HashMap;

use cranelift_codegen::ir::{AbiParam, ExtFuncData, Function, Signature, UserExternalNameRef, types};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::Context;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::value::Value;
use crate::MvmFunction;

#[repr(C)]
pub struct VmContext {
    pub stack_ptr: *mut Value,
    pub stack_len: usize,
    pub stack_cap: usize,
    pub locals_ptr: *mut Value,
    pub locals_len: usize,
    pub pc: usize,
    pub current_func: usize,
    pub code_ptr: *const u8,
    pub code_len: usize,
    pub exit_code: i64,
    pub halted: bool,
    pub strings_ptr: *const String,
    pub strings_len: usize,
    pub memory_ptr: *mut Value,
    pub memory_len: usize,
}

impl VmContext {
    pub const OFFSET_STACK_PTR: usize = 0;
    pub const OFFSET_STACK_LEN: usize = 8;
    pub const OFFSET_STACK_CAP: usize = 16;
    pub const OFFSET_LOCALS_PTR: usize = 24;
    pub const OFFSET_LOCALS_LEN: usize = 32;
    pub const OFFSET_PC: usize = 40;
    pub const OFFSET_CURRENT_FUNC: usize = 48;
    pub const OFFSET_CODE_PTR: usize = 56;
    pub const OFFSET_CODE_LEN: usize = 64;
    pub const OFFSET_EXIT_CODE: usize = 72;
    pub const OFFSET_HALTED: usize = 80;
    pub const OFFSET_STRINGS_PTR: usize = 88;
    pub const OFFSET_STRINGS_LEN: usize = 96;
    pub const OFFSET_MEMORY_PTR: usize = 104;
    pub const OFFSET_MEMORY_LEN: usize = 112;
}

pub struct JitTable {
    entries: HashMap<usize, *mut ()>,
}

impl JitTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn insert(&mut self, func_idx: usize, ptr: *mut ()) {
        self.entries.insert(func_idx, ptr);
    }

    pub fn get(&self, func_idx: usize) -> Option<*mut ()> {
        self.entries.get(&func_idx).copied()
    }
}

pub(crate) mod opcodes;
pub use self::opcodes::compile_function;

pub struct JitCompiler {
    module: JITModule,
    func_ids: HashMap<usize, FuncId>,
}

impl JitCompiler {
    pub fn new() -> Result<Self, String> {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names())
            .map_err(|e| format!("Failed to create JITBuilder: {:?}", e))?;
        let module = JITModule::new(builder);
        Ok(Self {
            module,
            func_ids: HashMap::new(),
        })
    }

    pub fn compile_function(
        &mut self,
        func: &MvmFunction,
        func_idx: usize,
    ) -> Result<*mut (), String> {
        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        let name = format!("jit_func_{}", func_idx);
        let func_id = self
            .module
            .declare_function(&name, Linkage::Export, &sig)
            .map_err(|e| format!("Failed to declare function: {:?}", e))?;

        let mut sig_i = Signature::new(CallConv::SystemV);
        sig_i.params.push(AbiParam::new(types::I64));
        sig_i.params.push(AbiParam::new(types::I64));

        let mut sig_ret_i = Signature::new(CallConv::SystemV);
        sig_ret_i.params.push(AbiParam::new(types::I64));
        sig_ret_i.returns.push(AbiParam::new(types::I64));

        let mut sig_f64 = Signature::new(CallConv::SystemV);
        sig_f64.params.push(AbiParam::new(types::I64));
        sig_f64.params.push(AbiParam::new(types::F64));

        let mut sig_ret_f64 = Signature::new(CallConv::SystemV);
        sig_ret_f64.params.push(AbiParam::new(types::I64));
        sig_ret_f64.returns.push(AbiParam::new(types::F64));

        let jit_push_int_id = self.module.declare_function("jit_push_int", Linkage::Import, &sig_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_push_f64_id = self.module.declare_function("jit_push_f64", Linkage::Import, &sig_f64)
            .map_err(|e| format!("{:?}", e))?;
        let jit_push_bool_id = self.module.declare_function("jit_push_bool", Linkage::Import, &sig_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_push_unit_id = self.module.declare_function("jit_push_unit", Linkage::Import, &sig_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_push_null_id = self.module.declare_function("jit_push_null", Linkage::Import, &sig_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_dup_id = self.module.declare_function("jit_dup", Linkage::Import, &sig_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_pop_int_id = self.module.declare_function("jit_pop_int", Linkage::Import, &sig_ret_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_pop_f64_id = self.module.declare_function("jit_pop_f64", Linkage::Import, &sig_ret_f64)
            .map_err(|e| format!("{:?}", e))?;
        let jit_pop_bool_id = self.module.declare_function("jit_pop_bool", Linkage::Import, &sig_ret_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_load_local_int_id = self.module.declare_function("jit_load_local_int", Linkage::Import, &sig_ret_i)
            .map_err(|e| format!("{:?}", e))?;
        let jit_store_local_int_id = self.module.declare_function("jit_store_local_int", Linkage::Import, &sig_i)
            .map_err(|e| format!("{:?}", e))?;

        let mut ctx = Context::for_function(Function::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, func_idx as u32),
            sig,
        ));

        let func_ctx = &mut ctx.func;
        let sig_ref_i = func_ctx.import_signature(sig_i);
        let sig_ref_ret_i = func_ctx.import_signature(sig_ret_i);
        let sig_ref_f64 = func_ctx.import_signature(sig_f64);
        let sig_ref_ret_f64 = func_ctx.import_signature(sig_ret_f64);

        let make_ext = |func_id: FuncId, sig_ref| ExtFuncData {
            name: cranelift_codegen::ir::ExternalName::User(UserExternalNameRef::from_u32(func_id.as_u32())),
            signature: sig_ref,
            colocated: false,
        };

        let jit_push_int_ref = func_ctx.import_function(make_ext(jit_push_int_id, sig_ref_i));
        let jit_push_f64_ref = func_ctx.import_function(make_ext(jit_push_f64_id, sig_ref_f64));
        let jit_push_bool_ref = func_ctx.import_function(make_ext(jit_push_bool_id, sig_ref_i));
        let jit_push_unit_ref = func_ctx.import_function(make_ext(jit_push_unit_id, sig_ref_i));
        let jit_push_null_ref = func_ctx.import_function(make_ext(jit_push_null_id, sig_ref_i));
        let jit_dup_ref = func_ctx.import_function(make_ext(jit_dup_id, sig_ref_i));
        let jit_pop_int_ref = func_ctx.import_function(make_ext(jit_pop_int_id, sig_ref_ret_i));
        let jit_pop_f64_ref = func_ctx.import_function(make_ext(jit_pop_f64_id, sig_ref_ret_f64));
        let jit_pop_bool_ref = func_ctx.import_function(make_ext(jit_pop_bool_id, sig_ref_ret_i));
        let jit_load_local_int_ref = func_ctx.import_function(make_ext(jit_load_local_int_id, sig_ref_ret_i));
        let jit_store_local_int_ref = func_ctx.import_function(make_ext(jit_store_local_int_id, sig_ref_i));

        let helpers = HelperFunctions {
            jit_push_int: jit_push_int_ref,
            jit_push_f64: jit_push_f64_ref,
            jit_push_bool: jit_push_bool_ref,
            jit_push_unit: jit_push_unit_ref,
            jit_push_null: jit_push_null_ref,
            jit_dup: jit_dup_ref,
            jit_pop_int: jit_pop_int_ref,
            jit_pop_f64: jit_pop_f64_ref,
            jit_pop_bool: jit_pop_bool_ref,
            jit_load_local_int: jit_load_local_int_ref,
            jit_store_local_int: jit_store_local_int_ref,
        };

        crate::jit::opcodes::compile_function(func, func_idx, &mut ctx, &helpers)?;

        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define function: {:?}", e))?;

        self.module
            .finalize_definitions()
            .map_err(|e| format!("Failed to finalize: {:?}", e))?;

        self.func_ids.insert(func_idx, func_id);

        let ptr = self.module.get_finalized_function(func_id);
        Ok(ptr as *mut ())
    }
}

pub struct HelperFunctions {
    pub jit_push_int: cranelift_codegen::ir::FuncRef,
    pub jit_push_f64: cranelift_codegen::ir::FuncRef,
    pub jit_push_bool: cranelift_codegen::ir::FuncRef,
    pub jit_push_unit: cranelift_codegen::ir::FuncRef,
    pub jit_push_null: cranelift_codegen::ir::FuncRef,
    pub jit_dup: cranelift_codegen::ir::FuncRef,
    pub jit_pop_int: cranelift_codegen::ir::FuncRef,
    pub jit_pop_f64: cranelift_codegen::ir::FuncRef,
    pub jit_pop_bool: cranelift_codegen::ir::FuncRef,
    pub jit_load_local_int: cranelift_codegen::ir::FuncRef,
    pub jit_store_local_int: cranelift_codegen::ir::FuncRef,
}

#[no_mangle]
pub extern "C" fn jit_push_int(ctx: *mut VmContext, val: i64) {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len >= ctx.stack_cap {
        panic!("JIT stack overflow");
    }
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        *slot = Value::Int(val);
    }
    ctx.stack_len += 1;
}

#[no_mangle]
pub extern "C" fn jit_push_f64(ctx: *mut VmContext, val: f64) {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len >= ctx.stack_cap {
        panic!("JIT stack overflow");
    }
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        *slot = Value::Float64(val);
    }
    ctx.stack_len += 1;
}

#[no_mangle]
pub extern "C" fn jit_push_bool(ctx: *mut VmContext, val: bool) {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len >= ctx.stack_cap {
        panic!("JIT stack overflow");
    }
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        *slot = Value::Bool(val);
    }
    ctx.stack_len += 1;
}

#[no_mangle]
pub extern "C" fn jit_push_unit(ctx: *mut VmContext) {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len >= ctx.stack_cap {
        panic!("JIT stack overflow");
    }
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        *slot = Value::Unit;
    }
    ctx.stack_len += 1;
}

#[no_mangle]
pub extern "C" fn jit_push_null(ctx: *mut VmContext) {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len >= ctx.stack_cap {
        panic!("JIT stack overflow");
    }
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        *slot = Value::Null;
    }
    ctx.stack_len += 1;
}

#[no_mangle]
pub extern "C" fn jit_dup(ctx: *mut VmContext) {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len == 0 {
        panic!("JIT stack underflow");
    }
    if ctx.stack_len >= ctx.stack_cap {
        panic!("JIT stack overflow");
    }
    unsafe {
        let src = ctx.stack_ptr.add(ctx.stack_len - 1);
        let dst = ctx.stack_ptr.add(ctx.stack_len);
        *dst = (*src).clone();
    }
    ctx.stack_len += 1;
}

#[no_mangle]
pub extern "C" fn jit_pop_int(ctx: *mut VmContext) -> i64 {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len == 0 {
        panic!("JIT stack underflow");
    }
    ctx.stack_len -= 1;
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        match *slot {
            Value::Int(v) => v,
            _ => panic!("jit_pop_int: expected Int"),
        }
    }
}

#[no_mangle]
pub extern "C" fn jit_pop_f64(ctx: *mut VmContext) -> f64 {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len == 0 {
        panic!("JIT stack underflow");
    }
    ctx.stack_len -= 1;
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        match *slot {
            Value::Float64(v) => v,
            _ => panic!("jit_pop_f64: expected Float64"),
        }
    }
}

#[no_mangle]
pub extern "C" fn jit_pop_bool(ctx: *mut VmContext) -> bool {
    let ctx = unsafe { &mut *ctx };
    if ctx.stack_len == 0 {
        panic!("JIT stack underflow");
    }
    ctx.stack_len -= 1;
    unsafe {
        let slot = ctx.stack_ptr.add(ctx.stack_len);
        match *slot {
            Value::Bool(v) => v,
            _ => panic!("jit_pop_bool: expected Bool"),
        }
    }
}

#[no_mangle]
pub extern "C" fn jit_load_local_int(ctx: *mut VmContext, idx: u32) -> i64 {
    let ctx = unsafe { &mut *ctx };
    let idx = idx as usize;
    if idx >= ctx.locals_len {
        panic!("jit_load_local_int: index {} out of bounds", idx);
    }
    unsafe {
        let slot = ctx.locals_ptr.add(idx);
        match *slot {
            Value::Int(v) => v,
            _ => panic!("jit_load_local_int: expected Int at index {}", idx),
        }
    }
}

#[no_mangle]
pub extern "C" fn jit_store_local_int(ctx: *mut VmContext, idx: u32, val: i64) {
    let ctx = unsafe { &mut *ctx };
    let idx = idx as usize;
    if idx >= ctx.locals_len {
        panic!("jit_store_local_int: index {} out of bounds", idx);
    }
    unsafe {
        let slot = ctx.locals_ptr.add(idx);
        *slot = Value::Int(val);
    }
}

#[no_mangle]
pub extern "C" fn jit_set_pc(ctx: *mut VmContext, pc: usize) {
    unsafe {
        (*ctx).pc = pc;
    }
}

#[no_mangle]
pub extern "C" fn jit_get_pc(ctx: *mut VmContext) -> usize {
    unsafe { (*ctx).pc }
}

pub fn build_vm_context(
    stack_ptr: *mut Value,
    stack_len: usize,
    stack_cap: usize,
    locals_ptr: *mut Value,
    locals_len: usize,
    pc: usize,
    current_func: usize,
    code_ptr: *const u8,
    code_len: usize,
    exit_code: i64,
    halted: bool,
    strings_ptr: *const String,
    strings_len: usize,
    memory_ptr: *mut Value,
    memory_len: usize,
) -> VmContext {
    VmContext {
        stack_ptr,
        stack_len,
        stack_cap,
        locals_ptr,
        locals_len,
        pc,
        current_func,
        code_ptr,
        code_len,
        exit_code,
        halted,
        strings_ptr,
        strings_len,
        memory_ptr,
        memory_len,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vm_context_layout() {
        let ctx_size = std::mem::size_of::<VmContext>();
        assert!(ctx_size > 0, "VmContext should have non-zero size");
    }

    #[test]
    fn test_jit_table_insert_get() {
        let mut table = JitTable::new();
        table.insert(0, 0x1234 as *mut ());
        assert_eq!(table.get(0), Some(0x1234 as *mut ()));
        assert_eq!(table.get(1), None);
    }
}
