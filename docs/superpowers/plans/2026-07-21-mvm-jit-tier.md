# MVM JIT Tier Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Cranelift-based JIT tier to the MVM bytecode interpreter, with profiling counters and automatic compilation of hot functions.

**Architecture:** 
- Phase 1 compiles hot functions (exec_count > 100) to native code using Cranelift JIT.
- JIT functions replace execute_loop for supported opcodes; unsupported opcodes and function calls fall back to the interpreter.
- VmContext carries VM state as flat pointers for efficient Cranelift access.
- The JIT function signature is `extern "C" fn(*mut VmContext) -> i64` (Phase 1 returns scalar only).

**Tech Stack:** Rust 2021, cranelift-jit 0.109, cranelift-codegen 0.109, cranelift-entity 0.109, target-lexicon 0.12

---

## Task 1: Add Cranelift dependencies

- [x] Edit `miva-vm/Cargo.toml` to add cranelift-jit, cranelift-codegen, cranelift-module, cranelift-entity, target-lexicon
- [x] Run `cargo check` in `miva-vm/` to verify dependencies resolve.

## Task 2: Create `miva-vm/src/jit/mod.rs`

- [x] Define `VmContext` struct with flat VM state
- [x] Define `JitTable` struct
- [x] Define `JitCompiler` struct wrapping `JITModule`
- [x] Implement `build_vm_context` 
- [x] Implement extern "C" helper functions: jit_push_int, jit_push_f64, jit_push_bool, jit_pop_int, jit_pop_f64, jit_pop_bool, jit_load_local_int, jit_store_local_int, jit_set_pc, jit_get_pc
- [x] Stub `compile_function` (returns trivial function, actual IR generation is Task 3)
- [x] Add unit tests for VmContext layout and JitTable

## Task 3: Create `miva-vm/src/jit/opcodes.rs`

- [ ] Implement `compile_function` with full opcode handling:
  - PushI64, PushF64, PushBool, PushNull, PushUnit
  - Dup, Drop
  - I64Add, I64Sub, I64Mul, I64Div, I64Rem
  - F64Add, F64Sub, F64Mul, F64Div
  - CmpEq, CmpNeq, CmpLt, CmpGt, CmpLe, CmpGe
  - Jmp, JmpIf, JmpIfNot
  - LoadLocal, StoreLocal
  - Ret, RetVal
- [ ] Implement `jit_compile` on `JitCompiler` that iterates program functions

## Task 4: Wire JIT into `miva-vm/src/lib.rs`

- [x] Add `pub mod jit;` to `miva-vm/src/lib.rs`

## Task 5: Modify `miva-vm/src/vm.rs` for profiling and JIT dispatch

- [x] Add `exec_counts: Vec<AtomicU64>` to `Mvm` struct
- [x] Add `jit_table: JitTable` and `jit_compiler: Option<JitCompiler>` to `Mvm`
- [x] Initialize JIT fields in `Mvm::with_program`
- [x] Add JIT dispatch hook in `call_internal`:
  - Check JitTable for compiled entry
  - Build VmContext, call JIT function
  - Sync back stack_len, pc, exit_code, halted
  - Fall back to interpreter if JIT returns -1
- [x] Add profiling counter increment and compilation trigger at threshold 100

## Task 6: Verification

- [x] Run `cargo check` in `miva-vm/` and `miva/`
- [x] Run `cargo test` in `miva-vm/`

---

## Design Notes

- **Phase 1 scope:** Only scalar (i64/f64/bool/unit) values flow through JIT. Complex values (String, Array, Struct, Closure) fall back to interpreter.
- **Calling convention:** JIT fn takes `*mut VmContext`, manipulates VM state directly via pointers. This avoids ABI issues with Value enum across the FFI boundary.
- **Return value:** JIT fn returns `i64` — Phase 1 only returns scalar values. Complex returns fall back.
- **Fallback safety:** Any unsupported opcode or value type triggers `jit_call_interpreter` which runs the full interpreter for the rest of the function.
