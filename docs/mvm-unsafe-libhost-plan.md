# Plan: MVM backend unsafe function support (single `libhost.so` per project)

## Goal

Make the MVM backend able to run unsafe functions:
- **Stdlib unsafe builtins** (`ptr_*`, `json_*`, `xml_*`, `toml_*`, `yaml_*`) — already native
  `MvmOp` opcodes. No change needed; they are the "native VM opcodes" path.
- **User `unsafe fn` with raw C `code`** (`DCFuncUnsafe`) — cannot be inlined into bytecode.
  Codegen emits a host call; the implementation lives in a single `libhost.so` per project,
  compiled at build time and `dlopen`'d by the `mvm` interpreter at run time.

Exactly **one** `libhost.so` per build, sitting next to `<name>.mvm`.

## Design overview

```
miva build -b mvm
  ├─ codegen mvm: emit <name>.mvm  (CallHost ops for user unsafe fns)
  └─ emit libhost.c from all DCFuncUnsafe defs
        └─ cc -shared -> build/<profile>/libhost.so   (one per project)

miva run -b mvm
  └─ mvm <name>.mvm
        └─ at load: dlopen sibling libhost.so, dlsym miva_host_<name> -> host_table
        └─ CallHost(name,arity): pop arity args, call host fn, push result
```

This is a WASM-style host-function model. The VM never compiles C; `cc` is invoked only by
the build step (same as the existing C/LLVM backends), and only for user-provided C.

## ABI

Shared header `miva-vm/include/mvp_host.h` (or inline into vm crate) defines:

```c
typedef struct { int8_t tag; /* 0 Int,1 F64,2 Bool,3 String,4 Null,5 Unit,... */ ... } MivaValue;
// constructor/maker helpers: mvp_int(i64), mvp_f64(f64), mvp_bool(bool), mvp_string(char*), ...
typedef MivaValue (*MivaHostFn)(const MivaValue* args, int argc);
```

Mirror the existing `Value` enum in `miva-vm/src/vm.rs` (Int/Float64/Float32/Bool/Char/String/
Null/Unit/Array/Struct/Box/Ptr/Future). Tag values must match exactly.

## Steps

### 1. Opcode: add `CallHost`
- `miva-vm/src/opcode.rs`: add `CallHost = 0xC0` with operands `(u32 name_idx, u8 arity)`.
  - `from_u8(0xC0)`, `operand_size() => 5`, `name() => "call_host"`.
- `miva/src/codegen/mvm.rs`: add `MvmOp::CallHost(u32, u8)` variant + emit (opcode + name
  string-pool index + arity). Reuse existing `resolve_string` for the function name.

### 2. Codegen: collect + emit host calls
- In `miva/src/codegen/mvm.rs` `build_ir` (mvm):
  - Walk `all_defs`; for each `Def::DCFuncUnsafe { name, params, returns, code, .. }`,
    record into a per-build host table (name, arity, optional return, raw C `code`).
  - When emitting a call to a user unsafe fn (resolved via `func_indices` / name lookup),
    emit `CallHost(name_idx, arity)` instead of `Call`. Stdlib `ptr_*` etc. keep their
    existing `CallBuiltin` opcodes (untouched).
  - Expose the collected host defs via `GeneratedOutput` (add field `host_defs: Vec<HostDef>`
    or a separate return), so `build.rs` can generate `libhost.c`.

### 3. Generate `libhost.c` at build time
- `miva/src/commands/build.rs` (MVM branch, ~line 800): after writing `.mvm`,
  - collect host defs from `output`;
  - if non-empty: emit `libhost.c` containing, for each def:
    ```c
    #include <mvp_host.h>
    MivaValue miva_host_<name>(const MivaValue* args, int argc) {
        /* unpack args[0..arity] via mvp_as_* helpers */
        <user C code>
        /* return via mvp_int/mvp_f64/... */
    }
    ```
    plus `#include <mvp_builtin.h>` and any needed includes.
  - compile: `cc -shared -fPIC -O2 libhost.c -o <build_dir>/libhost.so`
    (error clearly if `cc` missing: "MVM user unsafe functions require a C toolchain").
  - Respect `needs_build` caching (rebuild `.so` only when sources change).

### 4. VM: host table + dlopen
- `miva-vm/src/vm.rs`:
  - add `host_table: HashMap<String, MivaHostFn>` and `host_lib: Option<Library>` (from
    `libloading` or raw `dlopen`). Add `MivaHostFn` type alias.
  - `Mvm::with_host_lib(path)`: `dlopen` `libhost.so`, `dlsym` each known
    `miva_host_<name>` (names known from the bytecode's `CallHost` name strings), populate
    `host_table`. `dlopen` failure -> return `Err` (not panic), with actionable message.
  - `Opcode::CallHost(name_idx, arity)`: look up name in `program.strings`, find
    `host_table[name]`; pop `arity` `Value`s, convert to `MivaValue`, call fn, convert
    result back to `Value`, push. Missing entry -> `Err("unsafe host function '<name>'
    not found in libhost.so")`.
- `miva-vm/src/lib.rs`: expose `with_host_lib` and have the `mvm` binary (in `miva-vm/src/main.rs`
  or `bin`) auto-detect a sibling `libhost.so` next to the `.mvm` argument and load it.

### 5. Run wiring
- `miva/src/commands/run.rs` (MVM branch): nothing extra needed if the `mvm` binary
  auto-detects sibling `libhost.so`. Optionally pass explicit `--host libhost.so` arg.
- Ensure build emits `libhost.so` into the same `build/<profile>/` dir as `<name>.mvm`.

### 6. Value <-> MivaValue conversion
- Add `From<Value> for MivaValue` and `From<MivaValue> for Value` (or helper fns) in
  `miva-vm`, used by the `CallHost` handler. Cover Int/Float64/Float32/Bool/Char/String/
  Null/Unit at minimum; Array/Struct/Box by pointer pass-through if needed.

## Files to change

| File | Change |
|---|---|
| `miva-vm/src/opcode.rs` | `CallHost = 0xC0` variant + `from_u8`/`operand_size`/`name` |
| `miva/src/codegen/mvm.rs` | `MvmOp::CallHost`, collect unsafe defs, emit host calls |
| `miva/src/codegen/mod.rs` | `GeneratedOutput` gains host-def field (or new struct) |
| `miva/src/commands/build.rs` | MVM branch: generate + compile `libhost.c` -> `libhost.so` |
| `miva-vm/src/vm.rs` | `host_table`, `with_host_lib`, `CallHost` handler, value conversion |
| `miva-vm/src/lib.rs` (+ bin) | expose loader; auto-detect sibling `libhost.so` |
| `miva-vm/include/mvp_host.h` (new) | `MivaValue` ABI + helpers shared with generated C |

## Verification

- Add MVM-backend test: a project with a user `unsafe fn` (raw C) called from `main`,
  `miva build -b mvm` then `miva run -b mvm`; assert stdout.
- Add test: stdlib `ptr_*` still works on MVM (regression).
- Test with no user unsafe fns: build must NOT emit `libhost.so` (or emit empty/no-op).
- `miva-vm` unit test: `CallHost` with a Rust-registered host fn (no dlopen) returns correct value.
- Run existing MVM test suite (`mvm-backend-debug.md`) to confirm no opcode regressions.
