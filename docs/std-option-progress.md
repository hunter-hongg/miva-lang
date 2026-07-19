# std/option 实现进度 — **进行中**

> 目标：在 `stdlib/std-0.1.2/src/option.miva` 实现泛型 `Option[T]`，类似 Rust 的 `Option<T>`。
> 最近一次更新：2026-07-19。

---

## 0. TL;DR — **现状**

`option.miva` 文件已创建，前端解析通过（21 个定义）。全 77 项前端测试 + 339 项编译器测试通过（3 项预存 cxx codegen 问题无关）。

**`examples/option/` 样例已可编译并运行通过** —— `choose` 表达式 + `panic` 分支 + 跨分支不同返回类型 的 C++ lambda 推导问题已经解决（通过把外层 lambda 显式标注为 `expected_type`）。剩余阻塞：跨模块泛型枚举使用（typecheck 阶段 `enums` 映射只覆盖本模块定义）和 `Typ::TFunc` 缺失（高阶函数）。

| 层 | 状态 | 备注 |
|---|---|---|
| `stdlib/std-0.1.2/src/option.miva` 定义 | ✅ 已通过解析 | 含新增的 `some`/`none` 辅助构造函数 |
| 跨模块类型相等性 | ✅ 已修复 | `types_equal` 中按 basename 匹配，不依赖模块路径限定名 |
| C++ 关键词冲突 | ✅ 已修复 | 参数/变量已在编译器中通过 `mangle_cpp_kw` 进行处理 |
| `mvp_panic` noreturn | ✅ 已修复 | 在 headers 中标注 `[[noreturn]]`，以配合 lambda 类型推导 |
| 样例 `examples/option/` | ✅ 可编译并运行（cxx） | `choose` lambda 现已显式标注 `expected_type`，并按目标类型对值分支做 `static_cast` 收口 |
| 样例 `examples/option/` (LLVM) | ✅ 可编译并运行 | 修复 `var_seq` 命名空间后 LLVM 同样跑通；详见 §1.6 |
| 跨模块泛型枚举使用 | ❌ 受阻 | `std.option.is_some` 在仅包含当前模块枚举的 `enums` 映射中找不到 `std.option.Option` |
| 高阶函数（`map` 等） | ❌ 受阻 | 仍缺乏 `Typ::TFunc` |

---

## 1. 2026-07-19 改动

### 1.1 `stdlib/std-0.1.2/src/option.miva` — 额外辅助函数

新增：
- `some[T]` / `none[T]` — 用于构造 `Option` 值，避免直接引用枚举构造函数（适合跨模块使用）
- 移除了 `take` / `replace`：它们需要对 `ref` 参数进行修改操作，但 `ref` 是不可变的，且 Miva 不支持 `ref mut`

最终 API 表：

| 分类 | 函数 | 签名 | 说明 |
|------|------|------|------|
| **构造** | `some` | `(v: T): Option[T]` | 将值包装为 `Some` |
| | `none` | `(_dummy: T): Option[T]` | 创建 `None`（虚参承载 T） |
| **查询** | `is_some` | `(ref o: Option[T]): bool` | 是否为 `Some` |
| | `is_none` | `(ref o: Option[T]): bool` | 是否为 `None` |
| **解包** | `expect` | `(ref o: Option[T], msg: string): T` | 取值，失败时显示自定义消息 |
| | `unwrap` | `(ref o: Option[T]): T` | 取值，失败时 panic |
| | `unwrap_or` | `(ref o: Option[T], default: T): T` | 取值或返回默认值 |
| **比较** | `contains` | `(ref o: Option[T], x: T): bool` | 是否包含指定值 |
| **转换** | `flatten` | `(ref o: Option[Option[T]]): Option[T]` | 展平一层嵌套 |

### 1.2 类型检查器 — 跨模块类型相等性

类型相等性函数 `types_equal` 现在会剥离模块路径限定名：`std::option::Option` 与 `Option` 视为等价，比较时仅使用 basename（最末段）。参见 `miva/src/typecheck.rs:basename()`。

### 1.3 C++ 代码生成 — 关键词转义 + panic 返回

- 函数参数（`cxx_param`）、变量引用（`EVar`、`EMove`、`EClone`）现在均通过 `mangle_cpp_kw` 进行处理，因此 `default` 等 C++ 关键词即使出现在用户代码中也能正确处理。
- `cxx_choose` 现在对 `panic(...)` 分支发送裸语句（`mvp_panic(...);`）而非 `return mvp_panic(...);`，以避免 lambda 返回类型推导中的 `void`/`T` 冲突。
- `stdlib/mvp_builtin.h` 和 `~/.miver/repo/miva/v0.1.2/lib/mvp_builtin.h` 中的 `mvp_panic` 已标注 `[[noreturn]]`。

### 1.4 2026-07-19 后续 — `cxx_choose` 显式 `expected_type` 标注

让 `cxx_choose` 接受 `expected_type: Option<&str>`，由 `cxx_normal_func` / `cxx_async_func` / `cxx_main_func` / `cxx_func_inner` 透传函数返回类型；`cxx_expr` / `cxx_if` / `cxx_while` / `cxx_loop` / `cxx_for` / `cxx_call` / `cxx_binop` / `cxx_struct_lit` / `cxx_block` / `cxx_stmt` / `cxx_array_lit` 全部同步加上该参数（多数情况直接透传）。

效果：
- 外层 lambda 显式标注为 `([&]() -> <ret> { ... })()`，避免 `void`/`T` 推导歧义。
- 含 `panic` 的分支在 lambda 体里以 `mvp_panic(...); return <ret>();` 形式落地（`mvp_panic` 已 `[[noreturn]]`，所以 `return <ret>();` 不可达但满足 lambda 返回槽）。
- 值分支若自然 C++ 类型与 `ret` 不一致（例如 `bool` vs `mvp_builtin_boolean`）会被 `static_cast<ret>(...)` 收口，消除 `bool`/`signed char` 这类分支类型冲突。
- 当 `when` 是不带绑定的 `Option.Some` 时（解析为 `EFieldAccess(EVar("Option"), "Some")`）也走 tag 比对路径：`o.__tag == Option_Some_tag()`。原来只有带绑定的 `EEnumPattern` 走 tag 比对，不带绑定的会被错误地生成 `o == Option_Some()`，触发模板重载歧义。

`is_panic` 现在也会识别 `EBlock { stmts: [Stmt::SExpr(ECall "panic")], result: None }`，因为 `when (...) { panic(...); }` 在 AST 中是块包调用。

### 1.6 2026-07-19 后续 — LLVM 后端变量命名冲突

切到 `-b llvm` 跑同一份样例时，llc 报：

```
llc: error: build/debug/cache/src/main.miva.ll:1308:3:
error: multiple definition of local value named 's.reload.19'
```

根因：`LlvmCtx` 同时维护两套 reload 命名空间——
- `declare_var`（`SLet` / `for` 迭代）用 `var_decls`（per-name）生成 `s.reload.N`；
- `SAssign` 与 `emit_fresh_loads` 用 `tmp_counter`（global）生成 `s.reload.N`。

第 20 个 `SLet s`（suffix 19）会跟早先某个 `SAssign s` 撞车（后者 suffix 也恰好是 19）。

修复：把 `var_decls` 重命名为 `var_seq`，同时把 `declare_var` / `SAssign` / `emit_fresh_loads` 三处全部统一改用 `var_seq`（per-name，但 SAssign / fresh-load 也会推进它），并把格式从 `s.reload.N` 改为 `s.r.N`，确保 reload 跟 addr（`s.addr.N`）也不会再撞。

验证：
- `cargo build --release` 通过；
- `miva run -b llvm` 跑 `examples/option` 完整输出（is_some / is_none / 模式匹配 / unwrap / unwrap_or / expect / contains / 除法 / safe_div）全部正确；
- `cargo test --release` 通过 339 项，3 项预存失败（cxx codegen `const&` 断言）保持不变。

### 1.7 已知阻碍

**阻碍 A：跨模块泛型枚举未纳入 `enums` 映射**
`enums` 映射（`HashMap<String, Vec<EnumVariant>>`）仅由当前模块的定义填充（`build_enum_maps`）。当主模块调用 `std.option.is_some[int](a)` 时，类型检查器需要定位 `Option` 枚举以解析变体引用，但传入的类型名 `std::option::Option` 不在映射中。`types_equal` 修复部分缓解了该问题（类型现在可以按 basename 匹配），但枚举查找本身仍被阻止，并在 `typecheck.rs:869` 触发 `E0018`。

**阻碍 B（已解决）：`choose` 分支返回值与 `panic` 混合时的 lambda 类型推导失败**
原本在 `safe_div` 中 `true` 分支返回 `(a / b)`（`long int`）而 `panic` 分支发的是裸 `mvp_panic(...)`（`void`），外层 lambda 推导时遇到 `void`/`long int` 冲突而失败。已通过显式 `expected_type` 标注 + phantom `return <ret>();` 解决。

---

## 2. 待办项

| 项目 | 状态 | 说明 |
|------|------|------|
| `option.miva` 文件 | ✅ 已通过解析 | 10 个函数 + 类型定义 + exports |
| 前端 / 编译器测试 | ✅ 通过 | 77 + 339，无回归（3 项预存失败为 cxx_param `const&` 用例，与本次改动无关） |
| `examples/option/` 样例 | ✅ 可编译并运行（cxx + llvm） | `choose` + `panic` lambda 推导问题已解决；LLVM 端修好 `var_seq` 命名空间冲突 |
| 跨模块泛型枚举 | ❌ 区块 | 由阻碍 A 阻塞（`enums` 映射未导入） |
| `MIVA_STD` 环境变量文档 | ❌ 待办 | `MIVA_STD=/path/to/stdlib miva run` |
| 高阶函数（`map`/`and_then`） | ❌ 受阻 | 需要 `Typ::TFunc` |
| 除法算子 `/` | ✅ 已存在 | 无需额外工作 |

---

## 3. 风险 / 遗留

- **`enums` 映射未处理导入的枚举**：`typecheck.rs` 中的 `build_enum_maps` 只处理当前模块的 `Def`。要支持跨模块变体引用，要么（a）扩展 `enums` 映射以包含来自导入模块的枚举，要么（b）改用基于字符串路径的查找，而不是对枚举进行映射查找（类似 struct 的处理方式）。
- **`choose` lambda 现在总是显式标注 `expected_type`（当调用方已知）**：这会在 `cxx_choose` 的每个值分支引入 `static_cast<ret>(...)` 包装，绝大多数情况无副作用（`static_cast<int>(int_val)` 是恒等变换），但对复杂类型/带副作用的表达式需留意是否被重复求值。当前 `cxx_choose` 的分支体本身是单表达式（无副作用语义保证），所以现状安全。
- **除法已完整实现**：若用户遇到除法不工作，可能是运行时环境问题（如 `MIVA_STD` 未设置）而非编译器缺失。
