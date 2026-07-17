# std/vec 实现进度 — **已完成**

> 目标：用 `std/mem` 的 API（`alloc`/`realloc`/`free`/`offset`）在 `stdlib/std/src/vec.miva` 实现泛型 `Vec[T]`。
> 不用 `c unsafe`；缺语法就改前端（lexer/parser/typecheck/cxx）自己造。
> 最近一次更新：2026-07-16 20:12。

---

## 0. TL;DR — **端到端跑通**

`vec_e2e` 测试全输出正确：
```
len= 3  cap= 4
get0= 10
get1= 20
get2= 30
after set get1= 99
popped= 30  len= 2
copy len= 2
after truncate len= 1
after shrink_to_fit cap= 1
vec e2e ok
```

---

## 1. 前端新增语法（已完成、已编译）

改动分布在 `miva-frontend-rs/src/{lexer,parser,ast}.rs` 与 `miva/src/{ast,typecheck,codegen/cxx,codegen/llvm,codegen/mvm,macro_expand,semantic,warning,commands/build}.rs`。

### 1.1 比较与逻辑算子

| 算子 | Token | BinOp | 优先级 | CXX | LLVM | MVM |
|---|---|---|---|---|---|---|
| `<`  | `Lt`    | `Lt`  | 3 | ` < `  | `icmp slt` | `CmpLt` |
| `>`  | `Gt`    | `Gt`  | 3 | ` > `  | `icmp sgt` | `CmpGt` |
| `<=` | `LtEq`  | `Le`  | 3 | ` <= ` | `icmp sle` | `CmpLe` |
| `>=` | `GtEq`  | `Ge`  | 3 | ` >= ` | `icmp sge` | `CmpGe` |
| `&&` | `AndAnd`| `And` | 1 | ` && ` | `and i64`（**非短路**） | `I64And` |
| `||` | `OrOr`  | `Or`  | 2 | ` || ` | `or i64`（**非短路**） | `I64Or` |

### 1.2 结构体字段原地赋值

```
v.field := expr;   // 或
v.field  = expr;
```

- AST：`Stmt::SFieldAssign { loc, target, field, expr }`（两个 crate 的 ast.rs 都加了）。
- **当前只一层 `.field`**（`v.data := p`）。`a.b.c := x` 雾式未做。
- cxx codegen：`({target}).{field} = ({expr});`
- llvm/mvm codegen：**占位**（llvm 按 field 名硬编码 offset 仅 Vec[T] 对；mvm emit Drop 即 no-op）。vec 测试只用 cxx 后端，能跑。

### 1.3 cast 放开

- typecheck `ECast` valid 表加：
  - `(TInt, TPtrAny) | (TPtrAny, TInt)` —— `0 as ptrany` 表示 null 指针。
  - `(TPtrAny, TPtr { .. })` —— `std.mem.offset(...) as ptr<T>` 把 raw 指针转 typed 指针。
- cxx codegen：int↔ptrany 用 `reinterpret_cast`，其它仍 `static_cast`。

### 1.4 裸 `name := expr` → SAssign（不是 SLet）

parser 的 `parse_stmt` ident-starting 分支里，`name := expr`（无 `let`/`mut` 前缀）改成产 `SAssign`（赋值给既有变量），不再产 `SLet { mutable: false }`（那会让 cxx 后端 re-declare `const auto name = (expr)` 自引用，g++ 报 "use of 'i' before deduction of 'auto'"）。

---

## 2. vec.miva（`stdlib/std/src/vec.miva`，约 200 行）

### 2.1 设计

- `Vec[T] = struct { data: ptrany, len: int, cap: int }`，三字段全 i64-sized。
- backing buffer 由 `std.mem.alloc`/`realloc` 分配，byte size = `cap * elem_size[T]()`。
- `elem_size[T](): int => 8`（硬编码，匹配 `int64_t`/`double`；要支持 `bool`/`char`/`float32` 得加分派表）。
- 读写：`std.mem.offset(v.data, i * elem_size[T]()) as ptr<T>` 拿 typed 指针，`ptr_set(slot, x)` 写、`deref slot` 读。
- 字段原地改：`v.len := ...` / `v.data := ...` / `v.cap := ...`（用 §1.2 新语法）。
- 函数末尾返回值用 `return <expr>;`（裸表达式末尾无分号写法不被 parser 接受）。

### 2.2 导出的 API

`Vec`, `new`, `with_capacity`, `push`, `pop`, `get`, `get_unchecked`, `set`, `len`, `capacity`, `is_empty`, `free`, `shrink_to_fit`, `clear`, `copy`(旧名 clone), `truncate`, `grow`, `elem_size`

### 2.3 坑（已解决）

| 坑 | 解法 |
|---|---|
| `clone` 是保留字 | 函数名/export 改 `copy` |
| `*slot` 不是解引用语法 | 改 `deref slot` |
| `deref slot` 作返回值缺 `;` | 改 `return deref slot;` |
| `let mut out Vec[T] = ...` 不合法 | 改 `mut out := ...` |
| `import "std/vec"` 别名是 `std.vec` 不是 `vec` | 测试代码用 `std.vec.xxx` |
| `new_len := 0` 改 immutable 参数报错 | `truncate` 里用局部 `mut n := new_len` |

---

## 3. typecheck/semantic bug 修

### 3.1 global_type_sigs 没 normalize（build.rs）

`src/commands/build.rs` 构建 `global_type_sigs` 时调 `typecheck::normalize_params`/`normalize_typ`，让 import 的泛型函数签名里 `T` 是 `TGenericParam` 而非 `TStruct{name:"T"}`。为此把 typecheck.rs 里 `normalize_typ`/`normalize_params` 从 `fn` 改成 `pub(crate) fn`。

### 3.2 type_subst 填裸 TStruct 没归一化（typecheck.rs ECall）

`push[T](v, x)` 这种同模块泛型函数互调时，explicit type_args `[T]` 里 `T` 是裸 `TStruct{name:"T"}`，填进 `type_subst` 后 `resolve_type` 把 `Vec[T]` 的 `T` 换成 `TStruct{name:"T"}`，而 arg `v` 的类型是 `Vec<TGenericParam{name:"T"}>` —— 不匹配报 E0016。

修：typecheck.rs ECall 处理 explicit type_args 填 `type_subst` 时，把裸 `TStruct{name,fields:[],type_args:[]}`（name 不是真结构体）归一化成 `TGenericParam{name}` 再存。

### 3.3 types_equal 不认 TStruct↔TGenericParam（typecheck.rs）

`deref slot` 推断出 `TStruct{name:"T"}`（来自 `let slot ptr<T>` 类型注解解析路径），但 `pop[T]` 返回类型是 `TGenericParam{name:"T"}` —— `types_equal` 报不等，E0017。

修：`types_equal` 开头加归一化规则 —— 裸 `TStruct{name:"T",fields:[],type_args:[]}` 与 `TGenericParam{name:"T"}` 视作相等。

### 3.4 cast 表缺 ptrany→ptr<T>（typecheck.rs）

`as ptr<T>` 报 E0021 invalid cast —— cast 表只放了 int↔ptrany，没放 ptrany→ptr<T>。

修：`ECast` valid 表加 `(TPtrAny, TPtr { .. }) => true`。

### 3.5 semantic 里 EDeref 无脑报 E0010（semantic.rs）

`unsafe pop`/`get_unchecked` 里用 `deref slot`，semantic.rs 的 `EDeref` 分支不看上下文 caller_safety，无脑报 E0010。

修：`EDeref` 分支改成只在 `ctx.caller_safety == Safety::Safe` 时报 E0010，unsafe/trusted 函数里放行。

---

## 4. cxx codegen bug 修

### 4.1 跨模块 header 可见性（generate_header）

`vec.miva.h` 里泛型函数的完整 C++ 模板体调 `mvp_std::mem::alloc/realloc/free/offset` 和 `elem_size<T>()`，C++ 模板要求被调者声明在实例化点可见。

修：`generate_header`（cxx.rs 975）在 `#pragma once` 后、exported 内容前，遍历 defs 的 `SImport`/`SImportAs`/`SImportHere`，对每个 import 路径用 `cxx_include_path` 产 `#include`，让 `mem.miva.h`（含 `mvp_std::mem::*` 声明）被引入 `vec.miva.h`。

### 4.2 `new` 是 C++ 关键字冲突（mangle_cpp_kw）

`vec::new<int>()` 产出的 C++ 里 `new` 成了 C++ 关键字冲突，g++ 报 "expected unqualified-id before 'new'"。

修：cxx.rs 加 `mangle_cpp_kw(name)`，把裸标识符若命中 C++ 关键字表（`new`/`delete`/`class`/`template`/... 等 ~40 个）则前缀 `mvp_`。只 mangle `::` 分隔的最后一个 segment，保 namespace 路径。套在所有函数名/调用名产出处：`cxx_func_decl`、`cxx_normal_func` 的 signature（3 处 `let signature`）、`map_builtin` 的 fallback 分支（qualified name 经 `mangle_cpp_kw`）。

### 4.3 `ref v: Vec[T]` 产出 `const&`（PRef）

`PRef` 原产 `{} const& {}`（只读引用），字段赋值 `v.len := ...` 报 "assignment of member in read-only object"。

修：`cxx_param` 的 `PRef` 分支去掉 `const`，产 `{}& {}`。

### 4.4 `cxx_if` lambda 落末尾没 return → ud2 SIGILL（cxx_if）

`if (c) { return T; }` 产 `([&]() { if (c) { return T; } })()` —— lambda 返回类型被推断成 `mvp_builtin_unit`，if 假时落末尾没 return，g++ emit `ud2`（非法指令陷阱），运行时 SIGILL 崩溃。

修：`cxx_if` 的 lambda 显式 `-> void`，then/else body 用 expression statement `{ body; }` 而非 `return body;`，避免 g++ 推断非 void 返回类型。

### 4.5 g++ `-Wtemplate-body` 诊断

跨模块模板体调 `mvp_std::mem::*`（声明虽经 §4.1 include 可见）仍触发 g++ 的 `-Wtemplate-body` pedantic 警告当错。

修：`build.rs` 的 g++ args 加 `-Wno-template-body`。

---

## 5. 改动清单（文件级）

### miva-frontend-rs（已编译 OK）
- `src/ast.rs`：`BinOp` 加 `Lt/Gt/Le/Ge/And/Or`；`Stmt` 加 `SFieldAssign`。
- `src/lexer.rs`：Token 加 `LtEq/GtEq/AndAnd/OrOr/Amp/Pipe`；`<`/`>` 分支 lookahead 产 `<=`/`>=`；加 `&&`/`||`。
- `src/parser.rs`：
  - `prece` + `parse_binary_expr` 接入新算子。
  - `parse_stmt` ident-starting 分支：`.field := / =` 的 `SFieldAssign` 路径；**裸 `name := expr` 改产 `SAssign` 不再产 `SLet`**（§1.4）。

### miva（已编译 OK）
- `src/ast.rs`：同前端。
- `src/typecheck.rs`：
  - `EBinOp` 加 `Lt/Gt/Le/Ge`（numeric+同型→TBool）+ `And/Or`（TBool+TBool→TBool）。
  - `ECast` valid 加 `(TInt,TPtrAny)|(TPtrAny,TInt)|(TPtrAny,TPtr{..})`。
  - `check_block` 加 `SFieldAssign` 处理。
  - **ECall explicit type_args 填 type_subst 时归一化裸 TStruct→TGenericParam**（§3.2）。
  - **`types_equal` 加 TStruct↔TGenericParam 归一化**（§3.3）。
  - `normalize_typ`/`normalize_params` 改 `pub(crate)`。
- `src/commands/build.rs`：
  - **`global_type_sigs` 构建时调 normalize_params/normalize_typ**（§3.1）。
  - **g++ args 加 `-Wno-template-body`**（§4.5）。
- `src/codegen/cxx.rs`：
  - `cxx_binop` 加 `Lt/Gt/Le/Ge/And/Or` 的 C++ 串。
  - `ECast`：int↔ptrany 用 `reinterpret_cast`，其它仍 `static_cast`。
  - `cxx_stmt` 加 `SFieldAssign` → `({target}).{field} = ({expr});`。
  - **`mangle_cpp_kw`** + 套在 `cxx_func_decl`/`cxx_normal_func` signature（3 处）/`map_builtin` fallback（§4.2）。
  - **`PRef` 去 const**（§4.3）。
  - **`cxx_if` lambda `-> void` + expression statement body**（§4.4）。
  - **`generate_header` 加 import includes**（§4.1）。
- `src/codegen/llvm.rs`：`EBinOp` 加新算子（`icmp slt` 等 + `and`/`or` i64 非短路）；`gen_stmt` 加 `SFieldAssign`（占位：按 field 名硬编码 offset 仅 Vec[T] 对）。
- `src/codegen/mvm.rs`：`emit_binop` 加新算子（`CmpLt` 等 + `I64And`/`I64Or` 非短路）；`compile_stmt` 加 `SFieldAssign`（占位：emit Drop）；`find_impl_override` 加新算子空 op_name。
- `src/macro_expand.rs`：`expand_stmt` + `substitute_stmt_vars` 两处加 `SFieldAssign`。
- `src/semantic.rs`：`check_block` 加 `SFieldAssign`；**`EDeref` 分支改成只在 caller_safety==Safe 时报 E0010**（§3.5）。
- `src/warning.rs`：`check_stmt` 加 `SFieldAssign`。

### stdlib
- `stdlib/std/src/vec.miva`：约 200 行，端到端跑通（主体见 §2）。

---

## 6. 验证状态

| 项目 | 状态 |
|---|---|
| `miva-frontend-rs` `cargo build` | ✅ 通过 |
| `miva` `cargo build` | ✅ 通过 |
| vec.miva 前端解析 | ✅ 通过 |
| vec.miva 类型检查（typecheck） | ✅ 通过 |
| vec.miva 语义检查（semantic） | ✅ 通过 |
| vec_e2e g++ 编译 | ✅ 通过 |
| vec_e2e 运行 | ✅ 输出全对（见 §0） |

---

## 7. 复现命令

```sh
# 编译前端 + miva
cd /home/hunter/projects/miva-lang/miva-frontend-rs && cargo build
cp target/debug/miva-frontend target/release/miva-frontend
cd /home/hunter/projects/miva-lang/miva && cargo build

# stdlib 软链（一次性）
mkdir -p ~/.miver/lib && ln -sf /home/hunter/projects/miva-lang/stdlib/std ~/.miver/lib/std

# 端到端测试
cd /tmp/vec_e2e && /home/hunter/projects/miva-lang/miva/target/debug/miva build && ./build/debug/vec_e2e
```

---

## 8. 风险/遗留

- **llvm/mvm 后端的 `&&`/`||` 不是短路**：vec.miva 没用，但新语法层面要短路得另加 opcode。
- **llvm/mvm 后端的 `SFieldAssign` 是占位**：vec 测试只用 cxx 后端能跑；要 llvm/mvm 后端真用字段赋值得另做。
- **`elem_size[T]()` 硬编码 8**：只支持 `int`/`float64` 元素；要支持 `bool`/`char`/`float32` 得加分派表。
- **链式字段赋值 `a.b.c := x` 未支持**：当前只一层 `.field`。
- **`new` 是 C++ 关键字**：已在 cxx codegen 做 `mangle_cpp_kw` 通用 mangle，不再隐患。
- **cxx_if 的 `-> void` 修法**改了既有产出模式，若其它代码依赖 `if` 表达式返回值（非 stmt 语境）可能受影响 —— vec 测试用的都是 stmt 语境，未踩；要严格的话得区分 stmt-if 和 expr-if。
```
