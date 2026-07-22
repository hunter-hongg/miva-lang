use std::collections::HashMap;

use cranelift_codegen::cursor::{Cursor, FuncCursor};
use cranelift_codegen::ir::{Block, InstBuilder, MemFlags, types};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::Context;

use crate::jit::{HelperFunctions, VmContext};
use crate::MvmFunction;

const SUPPORTED_OPCODES: &[u8] = &[
    0x00, 0x01, 0x02, 0x04, 0x07, 0x08,
    0x09, 0x0A,
    0x0D, 0x0E,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A,
    0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
    0x25, 0x26, 0x27, 0x28, 0x29, 0x2A,
    0x30, 0x31, 0x32,
    0x35, 0x36,
];

fn load_field(cur: &mut FuncCursor, ctx: cranelift_codegen::ir::Value, offset: usize) -> cranelift_codegen::ir::Value {
    let const_offset = cur.ins().iconst(types::I64, offset as i64);
    let addr = cur.ins().iadd(ctx, const_offset);
    cur.ins().load(types::I64, MemFlags::trusted(), addr, 0)
}

fn store_field(cur: &mut FuncCursor, ctx: cranelift_codegen::ir::Value, offset: usize, val: cranelift_codegen::ir::Value) {
    let const_offset = cur.ins().iconst(types::I64, offset as i64);
    let addr = cur.ins().iadd(ctx, const_offset);
    cur.ins().store(MemFlags::trusted(), val, addr, 0);
}

fn store_state_and_return(
    cur: &mut FuncCursor,
    ctx: cranelift_codegen::ir::Value,
    pc: cranelift_codegen::ir::Value,
    exit_code: cranelift_codegen::ir::Value,
    halted: cranelift_codegen::ir::Value,
    ret_val: i64,
) {
    store_field(cur, ctx, VmContext::OFFSET_PC, pc);
    store_field(cur, ctx, VmContext::OFFSET_EXIT_CODE, exit_code);
    store_field(cur, ctx, VmContext::OFFSET_HALTED, halted);
    let const_ret = cur.ins().iconst(types::I64, ret_val);
    cur.ins().return_(&[const_ret]);
}

pub fn compile_function(
    _func: &MvmFunction,
    _func_idx: usize,
    ctx: &mut Context,
    helpers: &HelperFunctions,
) -> Result<(), String> {
    let func_ctx = &mut ctx.func;
    let mut cur = FuncCursor::new(func_ctx);

    let entry_block = cur.func.dfg.make_block();
    cur.insert_block(entry_block);
    let ctx_param = cur.func.dfg.append_block_param(entry_block, types::I64);

    let dispatch_block = cur.func.dfg.make_block();
    let check_end_block = cur.func.dfg.make_block();
    let dispatch_ok_block = cur.func.dfg.make_block();
    let ret_block = cur.func.dfg.make_block();
    let unsupported_block = cur.func.dfg.make_block();

    let mut handler_blocks: HashMap<u8, Block> = HashMap::new();
    for &opcode_byte in SUPPORTED_OPCODES {
        let block = cur.func.dfg.make_block();
        handler_blocks.insert(opcode_byte, block);
    }

    cur.ins().jump(dispatch_block, &[]);

    cur.insert_block(dispatch_block);

    let halted = load_field(&mut cur, ctx_param, VmContext::OFFSET_HALTED);
    let one = cur.ins().iconst(types::I64, 1);
    let is_halted = cur.ins().icmp(IntCC::Equal, halted, one);
    cur.ins().brif(is_halted, ret_block, &[], check_end_block, &[]);

    cur.insert_block(check_end_block);
    let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
    let code_len = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_LEN);
    let at_end = cur.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, pc, code_len);
    cur.ins().brif(at_end, ret_block, &[], dispatch_ok_block, &[]);

    cur.insert_block(dispatch_ok_block);
    let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
    let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
    let opcode_addr = cur.ins().iadd(code_ptr, pc);
    let opcode_val = cur.ins().load(types::I64, MemFlags::trusted(), opcode_addr, 0);
    let ff = cur.ins().iconst(types::I64, 0xFF);
    let loaded_opcode = cur.ins().band(opcode_val, ff);

    let pc_plus_1 = cur.ins().iadd(pc, one);
    store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, pc_plus_1);

    for &op_byte in SUPPORTED_OPCODES {
        let handler_block = handler_blocks[&op_byte];
        let next_block = cur.func.dfg.make_block();

        let opcode_const = cur.ins().iconst(types::I64, op_byte as i64);
        let cmp_val = cur.ins().icmp(IntCC::Equal, loaded_opcode, opcode_const);
        cur.ins().brif(cmp_val, handler_block, &[], next_block, &[]);

        cur.insert_block(next_block);
    }
    cur.ins().jump(unsupported_block, &[]);

    cur.insert_block(unsupported_block);
    let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
    let exit_code = load_field(&mut cur, ctx_param, VmContext::OFFSET_EXIT_CODE);
    let halted = load_field(&mut cur, ctx_param, VmContext::OFFSET_HALTED);
    store_state_and_return(&mut cur, ctx_param, pc, exit_code, halted, -1);

    cur.insert_block(ret_block);
    cur.ins().call(helpers.jit_push_unit, &[ctx_param]);
    let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
    let exit_code = load_field(&mut cur, ctx_param, VmContext::OFFSET_EXIT_CODE);
    let halted = load_field(&mut cur, ctx_param, VmContext::OFFSET_HALTED);
    store_state_and_return(&mut cur, ctx_param, pc, exit_code, halted, 0);

    cur.insert_block(handler_blocks[&0x00]);
    cur.ins().jump(dispatch_block, &[]);

    {
        let block = handler_blocks[&0x01];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let val = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let eight = cur.ins().iconst(types::I64, 8);
        let new_pc = cur.ins().iadd(pc, eight);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, val]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x02];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let val = cur.ins().load(types::F64, MemFlags::trusted(), addr, 0);
        let eight = cur.ins().iconst(types::I64, 8);
        let new_pc = cur.ins().iadd(pc, eight);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);
        cur.ins().call(helpers.jit_push_f64, &[ctx_param, val]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x04];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let val = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let ff = cur.ins().iconst(types::I64, 0xFF);
        let masked = cur.ins().band(val, ff);
        let one = cur.ins().iconst(types::I64, 1);
        let new_pc = cur.ins().iadd(pc, one);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);
        cur.ins().call(helpers.jit_push_bool, &[ctx_param, masked]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x07];
        cur.insert_block(block);
        cur.ins().call(helpers.jit_push_null, &[ctx_param]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x08];
        cur.insert_block(block);
        cur.ins().call(helpers.jit_push_unit, &[ctx_param]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x09];
        cur.insert_block(block);
        cur.ins().call(helpers.jit_dup, &[ctx_param]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x0A];
        cur.insert_block(block);
        cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x0D];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let idx = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let four = cur.ins().iconst(types::I64, 4);
        let new_pc = cur.ins().iadd(pc, four);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);
        let call_inst = cur.ins().call(helpers.jit_load_local_int, &[ctx_param, idx]);
        let val = cur.func.dfg.first_result(call_inst);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, val]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x0E];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let idx = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let four = cur.ins().iconst(types::I64, 4);
        let new_pc = cur.ins().iadd(pc, four);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);
        let call_inst = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let val = cur.func.dfg.first_result(call_inst);
        cur.ins().call(helpers.jit_store_local_int, &[ctx_param, idx, val]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x10];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().iadd(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x11];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().isub(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x12];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().imul(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x13];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().sdiv(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x14];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().srem(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x15];
        cur.insert_block(block);
        let call_inst = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst);
        let result = cur.ins().ineg(a);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x16];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().ishl(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x17];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().sshr(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x18];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().band(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x19];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().bor(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x1A];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().bxor(a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x1B];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().fadd(a, b);
        cur.ins().call(helpers.jit_push_f64, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x1C];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().fsub(a, b);
        cur.ins().call(helpers.jit_push_f64, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x1D];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().fmul(a, b);
        cur.ins().call(helpers.jit_push_f64, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x1E];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().fdiv(a, b);
        cur.ins().call(helpers.jit_push_f64, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x1F];
        cur.insert_block(block);
        let call_inst = cur.ins().call(helpers.jit_pop_f64, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst);
        let result = cur.ins().fneg(a);
        cur.ins().call(helpers.jit_push_f64, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x25];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().icmp(IntCC::Equal, a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x26];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().icmp(IntCC::NotEqual, a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x27];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().icmp(IntCC::SignedLessThan, a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x28];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().icmp(IntCC::SignedGreaterThan, a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x29];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x2A];
        cur.insert_block(block);
        let call_inst_b = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let b = cur.func.dfg.first_result(call_inst_b);
        let call_inst_a = cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        let a = cur.func.dfg.first_result(call_inst_a);
        let result = cur.ins().icmp(IntCC::SignedGreaterThanOrEqual, a, b);
        cur.ins().call(helpers.jit_push_int, &[ctx_param, result]);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x30];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let offset = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let four = cur.ins().iconst(types::I64, 4);
        let pc_plus_4 = cur.ins().iadd(pc, four);
        let new_pc = cur.ins().iadd(pc_plus_4, offset);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x31];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let offset = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let four = cur.ins().iconst(types::I64, 4);
        let new_pc = cur.ins().iadd(pc, four);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);

        let take_jmp = cur.func.dfg.make_block();
        let no_jmp = cur.func.dfg.make_block();

        let call_inst = cur.ins().call(helpers.jit_pop_bool, &[ctx_param]);
        let cond_val = cur.func.dfg.first_result(call_inst);
        let one = cur.ins().iconst(types::I64, 1);
        let cond_true = cur.ins().icmp(IntCC::Equal, cond_val, one);
        cur.ins().brif(cond_true, take_jmp, &[], no_jmp, &[]);

        cur.insert_block(take_jmp);
        let pc2 = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let new_pc2 = cur.ins().iadd(pc2, offset);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc2);
        cur.ins().jump(dispatch_block, &[]);

        cur.insert_block(no_jmp);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x32];
        cur.insert_block(block);
        let pc = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let code_ptr = load_field(&mut cur, ctx_param, VmContext::OFFSET_CODE_PTR);
        let addr = cur.ins().iadd(code_ptr, pc);
        let offset = cur.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let four = cur.ins().iconst(types::I64, 4);
        let new_pc = cur.ins().iadd(pc, four);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc);

        let take_jmp = cur.func.dfg.make_block();
        let no_jmp = cur.func.dfg.make_block();

        let call_inst = cur.ins().call(helpers.jit_pop_bool, &[ctx_param]);
        let cond_val = cur.func.dfg.first_result(call_inst);
        let zero = cur.ins().iconst(types::I64, 0);
        let cond_false = cur.ins().icmp(IntCC::Equal, cond_val, zero);
        cur.ins().brif(cond_false, take_jmp, &[], no_jmp, &[]);

        cur.insert_block(take_jmp);
        let pc2 = load_field(&mut cur, ctx_param, VmContext::OFFSET_PC);
        let new_pc2 = cur.ins().iadd(pc2, offset);
        store_field(&mut cur, ctx_param, VmContext::OFFSET_PC, new_pc2);
        cur.ins().jump(dispatch_block, &[]);

        cur.insert_block(no_jmp);
        cur.ins().jump(dispatch_block, &[]);
    }

    {
        let block = handler_blocks[&0x35];
        cur.insert_block(block);
        cur.ins().call(helpers.jit_push_unit, &[ctx_param]);
        cur.ins().jump(ret_block, &[]);
    }

    {
        let block = handler_blocks[&0x36];
        cur.insert_block(block);
        cur.ins().call(helpers.jit_pop_int, &[ctx_param]);
        cur.ins().jump(ret_block, &[]);
    }

    Ok(())
}
