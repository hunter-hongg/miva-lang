# MVM 后端调试进度

> 目标：让 `examples/vec` 在三个后端（CXX、LLVM、MVM）上输出一致的正确结果

## 总体状态

| 后端 | 状态 | 输出匹配参考 |
|------|------|-------------|
| CXX  | ✅ 完全正确 | ✅ |
| LLVM | ✅ 完全正确 | ✅ |
| MVM  | ✅ 完全正确 | ✅ |

---

## MVM 已修复的问题

### 1. `SFieldAssign` 是空操作

**症状**: 对 Vec struct 字段的赋值（如 `v.len = v.len + 1`）完全无效。

**根因**: MVM 的 `SFieldAssign` 代码生成器只写了 `compile_expr(target)` + `compile_expr(expr)`，两个值都被 drop。VM 端虽然有 `StructSet` opcode，但从未被调用。

**修复**:
- `miva/src/codegen/mvm.rs`: 添加 `Stmt::SFieldAssign` handler，编译 target + expr，发射 `StructSet`，然后将修改后的 struct `StoreLocal` 存回。
- `miva-vm/src/vm.rs`: `StructSet` opcode 原本 push 修改后的 struct — 已验证实现正确。

### 2. 缺少指针内置函数

**症状**: `ptr_set`, `ptr_ref`, `ptr_alloc`, `ptr_free`, `ptr_realloc`, `ptr_offset` 调用导致 `Drop` + `PushUnit`（未知函数路径）。

**根因**: `builtin_indices` HashMap 中没有这些内置函数的条目。

**修复**: 在两个地方添加了 `ptr_*` 内置函数：
- `miva/src/codegen/mvm.rs`: `builtin_indices` 表中添加了 `("ptr_alloc", 79)`...`("ptr_ref", 84)`。
- `miva-vm/src/vm.rs`: 内置函数表添加了对应条目（79-84），并在 `call_builtin` 中添加了处理器。

### 3. 内置函数名称不匹配

**症状**: stdlib 调用 `ptr_set(slot, x)` 和 `ptr_ref(slot)`，但内置函数表用 `ptr_set_i64` 和 `ptr_ref_i64`。

**根因**: 代码生成器用 `ptr_set_i64` / `ptr_ref_i64` 查找内置函数，但 stdlib 源码调用的是 `ptr_set` / `ptr_ref`。

**修复**: `miva/src/codegen/mvm.rs` — 将 builtin_indices 中的名称改为 `ptr_set` 和 `ptr_ref`（匹配 stdlib 和 CXX 后端）。

### 4. ref-param 返回逻辑误用于非 void 函数

**症状**: `len(v)` 返回 Vec struct 而非长度值；`offset(p, n)` 返回 struct 而非指针。MVM 运行时在 `I64Shr`/`I64Sub` 等处崩溃（因为栈上出现了 struct Value）。

**根因**: 新增的 ref-param 返回机制（void 函数返回修改后的 struct 给调用方）被错误地应用于所有带 ref 参数的函数，包括非 void 的 `len`、`capacity`、`is_empty`、`offset`。这些函数应该返回其表达式结果，而不是 ref 参数的值。

**修复**:
- `compile_function` 签名添加 `returns: &Option<Typ>` 参数。
- ref-param 的 `Drop` + `LoadLocal` + `RetVal` 仅在 `returns.is_none()`（即 void 函数）时执行。
- 调用方（`build_ir` pass 2）从 `Def::DFunc` 的 `returns` 字段提取并传入。

### 5. `realloc` 返回 Unit 而非指针

**症状**: `grow` 中调用 `realloc(v.data, new_size)` 后 `v.data` 变为 Unit，导致所有指针操作崩溃。

**根因**: `realloc(p, size) => ptr_realloc(p, size)` 是一个带表达式体的函数。MVM 的 `SExpr` handler 总是发出 `Drop`，所以 `ptr_realloc` 的返回值被丢弃。函数体编译结果为 `EBlock { stmts: [SExpr{ECall}], result: None }` — stmts 被 drop，result=None 发出 PushUnit。

**修复**: 在 `compile_function` 中添加了隐式返回逻辑（类似 CXX 后端的 `take_last_expr`）：对于非 void 函数，如果函数体是 `EBlock` 且 `result: None`，则最后一条语句（如果是 `SExpr`）被编译时不带尾随的 `Drop`，其值留在栈上，由后续的 `needs_ret` / `RetVal` 处理。

### 6. `I64And`/`I64Or` 在布尔值上崩溃

**症状**: `get` 函数中的 `(i < 0) || (i >= v.len)` 在运行时 panic。

**根因**: 比较操作（`CmpLt`/`CmpGe`）返回 `Value::Bool`，但 `I64And`/`I64Or` 直接调用 `as_i64().unwrap()`，遇到布尔值时返回 None → panic。

**修复**: `miva-vm/src/vm.rs` — `I64And` 和 `I64Or` 现在根据操作数类型分派：
- 两个 Bool → 逻辑 AND/OR，返回 Bool
- 两个 Int → 按位 AND/OR，返回 Int
- 混合类型 → 返回 Err

### 7. `StructSet` 返回 Unit 而非修改后的 struct

**症状**: `v.data := p` 执行后 `v` 中的数据变为 `Unit`，导致第二个 `StructSet`（`v.cap := new_cap`）报错 `"StructSet expected struct, got unit"`。

**根因**: VM 的 `StructSet` handler 在修改 struct 字段后：
```rust
fields[field_idx] = value;
self.push(Value::Unit);  // ← bug: 应该 push 修改后的 struct
```

**修复**: `miva-vm/src/vm.rs:816` — 改为 `self.push(Value::Struct(fields))`。

### 8. 隐式返回路径缺少 `RetVal`

**症状**: 非 void 函数返回 `Unit`（如 `realloc` 返回 `Unit` 而不是新指针），导致调用方拿到 `Unit` 并传给后续操作报错。

**根因**: `compile_function` 的隐式返回逻辑（`returns.is_some() && EBlock { result: None }`）在编译最后一条 SExpr 作为表达式后，直接 `return` 跳过了 `needs_ret` / `RetVal` 发射：
```rust
self.pop_scope();
return; // ← 跳过了 RetVal！
```
同时，这个 `return` 也跳过了函数表更新（`self.functions[func_idx].code = ...`），导致整个函数的字节码丢失。

受影响的函数：`realloc`, `offset` 等所有 body 为 `{ expr; }` 的非 void 函数。

**修复**:
- `miva/src/codegen/mvm.rs` — 在 `return;` 之前添加 `RetVal` 发射。
- 同时添加函数表更新，确保字节码被存入 `self.functions[func_idx].code`。

### 9. `ptr_*` 内置函数的单位错误（bytes vs Value slots）

**症状**: `ptr_offset(v.data, i * 8)` 返回 `+8` 而不是 `+1`（slot 索引），导致越界。`ptr_alloc(n)` 分配 n 个 Value slot 而非 n/8 个。

**根因**: `elem_size[T]()` 在 stdlib 中硬编码为返回 8（字节数/元素），但 MVM 的内存是 `Vec<Value>`（每个元素 1 个 slot）。直接的 byte→slot 映射导致分配和偏移量错误。

**修复**: `miva-vm/src/vm.rs` — `ptr_alloc` 和 `ptr_realloc` 将字节数除以 8（`bytes/8 + (bytes%8>0)`）来计算 slot 数；`ptr_offset` 将偏移量除以 8（`n/8`）来计算 slot 偏移。

### 10. `PtrLoad`/`PtrStore` 不支持 `Int` 作为内存指针

**症状**: `deref slot`（其中 `slot` 来自 `offset` 返回的 `ptrany`）报错 `"PtrLoad expected ptr, got int"`。

**根因**: `PtrLoad` 和 `PtrStore` 只处理 `Value::Ptr(idx, locals)`（指向局部变量的指针），但 `offset` 返回的 `ptrany` 是 `Value::Int`（指向原始内存的指针）。

**修复**: `miva-vm/src/vm.rs` — `PtrLoad` 和 `PtrStore` 现在同时处理：
- `Value::Ptr(idx, locals)` — 本地变量指针
- `Value::Int(addr)` — 原始内存指针（从 `self.memory[addr]` 读写）

### 11. `free` 函数名冲突（`vec.miva` vs `mem.miva`）

**症状**: 调用 `std.vec.free` 时实际调用的是 `std.mem.free`（因为 `func_indices` 用裸名 `"free"` 做 key，后者覆盖前者），导致传入 Vec struct 给期望 `ptrany` 的函数，运行时崩溃。

**根因**: `func_indices` 和 `func_ref_params` HashMap 使用裸函数名作为 key。当两个模块有同名函数时（如 `vec.miva` 和 `mem.miva` 都有 `free`），后注册的覆盖前者。

**修复**: `stdlib/std-0.1.1/src/mem.miva` 中将 `free` 重命名为 `mem_free`，避免与 `vec.miva` 的 `free` 冲突。

---

## 相关文件

| 文件 | 作用 |
|------|------|
| `miva/src/codegen/mvm.rs` | MVM 代码生成器（主要修改点） |
| `miva-vm/src/vm.rs` | MVM 运行时 |
| `miva-vm/src/opcode.rs` | 操作码定义 |
| `miva/src/ast.rs` | AST 定义（`Expr::EBlock`, `BinOp` 等） |
| `examples/vec/src/main.miva` | 测试用例 |
| `examples/vec/build/release/vec-demo.mvm` | 生成的字节码 |
| `stdlib/std-0.1.1/src/vec.miva` | Vec 标准库实现 |
| `stdlib/std-0.1.1/src/mem.miva` | 内存操作（`ptr_*` 内置函数包装） |
| `stdlib/std-0.1.1/src/str.miva` | 字符串标准库 |

## 相关提交

所有修改均为工作区未提交的更改。关键提交历史：
- `fac847d` — TOML/YAML stdlib + VM 运行时支持
- `0d51393` — JSON/XML stdlib + async/future 类型 + VM 运行时支持
- `15938e0` — 可插拔后端（cxx/llvm/mvm）+ miva-vm crate