# Miva 编程语言

Miva 是一种编译型系统编程语言。编译器可将 Miva 源码转译到 C++（默认后端），或到 LLVM IR，或到 Miva 虚拟机（MVM）字节码，并编译为原生二进制，或直接由解释器运行。它具有强静态类型系统、泛型编程、模式匹配、移动语义、安全级别系统、宏，以及零成本 C FFI。

## 目录

- [概述](#概述)
- [快速开始](#快速开始)
- [项目配置](#项目配置)
- [注释](#注释)
- [模块系统](#模块系统)
- [定义](#定义)
- [类型](#类型)
- [变量](#变量)
- [表达式](#表达式)
- [语句](#语句)
- [控制流](#控制流)
- [函数](#函数)
- [结构体](#结构体)
- [泛型](#泛型)
- [安全级别](#安全级别)
- [移动语义与所有权](#移动语义与所有权)
- [异步（Async）](#异步async)
- [方法调用语法糖](#方法调用语法糖)
- [宏](#宏)
- [内建函数](#内建函数)
- [标准库](#标准库)
- [C FFI（外部函数接口）](#c-ffi外部函数接口)
- [运算符重载](#运算符重载)
- [编译器流程与命令](#编译器流程与命令)
- [错误码](#错误码)
- [警告码](#警告码)

---

## 概述

Miva 是一个全栈的编程语言编译器：

1. **前端**（`miva-frontend-rs`）—— 对 `.miva` 源文件进行词法分析与语法分析，生成 JSON AST。
2. **编译器**（`miva`）—— 加载 JSON AST，执行宏展开、语义分析、类型检查，并为所选后端生成代码。
3. **后端** —— 三个后端之一（见下文）将生成的产物转换为原生二进制，或在 MVM 解释器上运行。

编译流程：

```
.miva 源文件 → 词法/语法分析 → JSON AST
  → 宏展开 → 符号表 → 语义分析
  → 类型检查 → 代码生成（C++ / LLVM IR / MVM 字节码） → 原生二进制 / MVM
```

### 后端

Miva 支持三个后端，可在每次构建时通过 `-b` 标志或 `miva.toml` 中的 `[project] backend` 字段选择：

| 后端 | `-b` 取值 | 产物 | 说明 |
|------|------------|------|------|
| **C++**（`cxx`） | `cxx` | 原生可执行文件 / `.so` | 默认。生成 C++20 并由 `g++` 编译。 |
| **LLVM**（`llvm`） | `llvm` | 原生可执行文件 / `.so` | 生成 LLVM IR，经 `llc` + `g++` 链接器编译。 |
| **MVM**（`mvm`） | `mvm` | `.mvm` 字节码 | 生成 Miva 虚拟机字节码，由 `mvm` 解释器运行（无需原生链接器）。 |

`cxx` 和 `llvm` 后端均生成原生二进制。`mvm` 后端生成可移植字节码，由内置的 `mvm` 解释器（`miva-vm`）执行，适合快速迭代和跨平台运行。

---

## 快速开始

### 安装

```bash
# 克隆仓库
git clone <repo-url>
cd miva-lang

# 构建前端与编译器
./build.sh --release
```

构建完成后，将 `miva-frontend-rs/target/release/miva-frontend` 与 `miva/target/release/miva` 加入你的 `PATH`。

### 创建新项目

```bash
# 初始化一个名为 "myapp" 的二进制项目
miva init myapp -t bin

# 初始化一个共享库项目
miva init mylib -t lib
```

`-t` 选择项目类型：`bin`（可执行文件）或 `lib`（共享库）。这会创建如下结构：

```
myapp/
├── miva.toml      # 项目配置
└── src/
    └── main.miva  # 入口（lib 项目为 src/lib.miva）
```

### 构建与运行

```bash
# 构建项目（默认 cxx 后端）
miva build --release

# 构建并运行
miva run --release

# 使用指定后端构建/运行
miva run -b llvm          # LLVM 后端
miva run -b mvm           # MVM 后端（别名：--mvm）
miva build -b mvm         # 生成 .mvm 字节码

# 编译单个文件（快速测试）
miva sin-build path/to/file.miva
miva sin-run path/to/file.miva

# 清理构建产物
miva clean

# 运行测试
miva test <test_file.miva>
```

### 你好，世界！

```miva
module main;

main = () => {
  println("Hello, World");
}
```

构建并运行：

```bash
miva build --release
miva run --release
```

---

## 项目配置

每个 Miva 项目根目录都必须有一个 `miva.toml`：

```toml
[project]
name = "myapp"
type = "bin"         # "bin" 为可执行文件，"lib" 为共享库
version = "0.1.0"
backend = "cxx"      # 可选：cxx（默认）、llvm 或 mvm

[env]

[scripts]
dev = "miva run -b mvm"
release = "miva build -b llvm --release"

[dependencies]
std = "0.1.2"        # 标准库依赖
```

### 项目类型

- **`bin`** —— 编译为带 `main()` 入口的原生可执行文件（使用 `src/main.miva`）。
- **`lib`** —— 编译为共享库（`.so`），带 `-fPIC` 和 `-shared` 标志。使用 `src/lib.miva` 作为入口。

### 后端选择

后端按如下优先级选择：`-b` / `--mvm` 命令行标志，其次为 `miva.toml` 中的 `[project] backend` 字段（默认 `cxx`）。后端细节见 [编译器流程与命令](#编译器流程与命令)。

### 脚本

`[scripts]` 段定义可用 `miva <name>` 运行的自定义命令。内建命令名（`init`、`build`、`run`、`clean`、`sin-build`、`sin-run`、`get`、`dep`、`test`、`reinit`）始终优先于脚本。

### 依赖

依赖从标准库路径获取。标准库以 `std-0.1.2` 形式随附：

```toml
[dependencies]
std = "0.1.2"
```

来自 GitHub 的依赖可用以下命令安装：

```bash
miva get <github-url>
```

---

## 注释

Miva 支持三种注释：

```miva
// 单行注释

/*
 * 多行块注释（支持嵌套）
 * /* 嵌套可用 */
 */

/! Magical 指令（控制编译器行为）
```

### Magical 指令

```miva
/! warning_off W0001    // 抑制警告 W0001
/! warning_err W0002    // 将警告 W0002 视为错误
/! release always       // 标记为仅 release 模式
/! mangle name          // 自定义名称修饰
```

### Intro 注释（注解）

```miva
@ unsafe: performs raw memory operations
@ usage: used as internal helper
@ param: x is the input value
@ impl: trait implementation for struct
@ trusted: safe wrapper around unsafe code
```

Intro 注释标注下一个定义，并会校验正确性（例如 `unsafe` 注解仅可出现在 `unsafe` 函数前，`usage` 可出现在任何定义前，等等）。

---

## 模块系统

每个 Miva 文件必须恰好声明一个模块：

```miva
module main;          // 简单模块
module std.io;        // 带命名空间的模块（在 C++ 中创建 mvp_std::io）
module my.app.utils;  // 深层命名空间
```

模块声明 **必须** 出现在文件顶部，位于任何其他定义之前。

### 导入

```miva
// 基础导入
import "std/str";

// 带命名空间别名的导入
import "std/io" as io;

// 导入并并入当前命名空间
import "std/io" as .;

// 导入 C 头文件（生成 #include <stdio.h>）
import "c:stdio.h";
```

导入解析：
- `proj_name/path` —— 项目内部：解析到 `src/path.miva`
- `std/path` —— 标准库：解析到标准库包含目录
- `library/path` —— 外部依赖
- `c:header.h` —— C 头文件（生成 `#include <header.h>`）

### 导出

```miva
export my_function;
export my_struct;
```

导出的符号对导入该文件的其他模块可见。泛型函数以 C++ 模板形式生成在头文件中；非泛型函数在头文件中声明、在源文件中定义。

---

## 定义

### 函数

```miva
// 简单函数（无返回值）
greet = () => {
  println("Hello!");
}

// 带参数和返回类型的函数
add = (a: int, b: int): int => {
  return a + b;
}

// 单表达式函数（无需花括号）
double = (x: int): int => x * 2

// 空返回（未指定返回类型）
log = (msg: string) => {
  prints(msg);
  print("\n");
}

// 带 ref（借用）参数的函数
print_len = (ref s: string) => {
  printlns!(string_length(s));
}
```

### 结构体

```miva
// 简单结构体
Point = struct {
  x: int,
  y: int,
}

// 空结构体
Empty = struct {}

// 带泛型类型参数的结构体
Box[T] = struct {
  value: T,
}

Pair[T, U] = struct {
  first: T,
  second: U,
}
```

### 结构体字面量

```miva
let p Point = struct Point { x = 10, y = 20 };
let b Box[int] = struct Box[int] { value = 42 };
```

### 测试

```miva
test test_name = (): int => {
  assert!(some_condition);
  0;
}
```

测试会被单独编译为测试可执行文件。它们必须返回 `int`。

---

## 类型

### 基础类型

| 类型 | 说明 | C++ 映射 |
|------|------|-----------|
| `int` | 有符号整数 | `mvp_builtin_int` |
| `bool` | 布尔值 | `mvp_builtin_boolean` |
| `float32` | 32 位浮点数 | `mvp_builtin_float` |
| `float64` | 64 位浮点数 | `mvp_builtin_float` |
| `char` | 字符（字节） | `mvp_builtin_byte` |
| `string` | 字符串 | `mvp_builtin_string` |

### 复合类型

| 类型 | 说明 | C++ 映射 |
|------|------|-----------|
| `array<T>` | T 的数组/向量 | `std::vector<T>` |
| `ptr<T>` | 指向 T 的指针 | `T*` |
| `box<T>` | 堆分配的 T 盒子 | `mvp_builtin_box<T>` |
| `ptrany` | 空指针 | `mvp_builtin_ptrany` |
| `null` | 空值/无值 | `void` |

### 结构体类型

结构体类型通过其名称引用，可带泛型类型实参：

```miva
let p Point;
let b Box[int];
let pair Pair[int, string];
```

---

## 变量

### 类型推断变量

```miva
// 类型推断的不可变变量
x := 42;

// 类型推断的可变变量
mut count := 0;

// 类型推断变量默认不可变
```

### 显式类型变量

```miva
let x int = 42;
let name string = "Miva";
let p Point = struct Point { x = 1, y = 2 };
```

### 赋值

```miva
mut x := 10;
x = 20;     // 正确：x 是可变的

// 错误：不能对不可变变量赋值
y := 10;
y = 20;     // 编译错误
```

### 移动与克隆

```miva
// 移动所有权
move x;                // x 被移动，之后不可再使用

// 克隆（复制）值
clone x;               // x 仍然有效
```

基础类型（int、bool、float32、float64、char）是复制类型，无需显式 `clone`。仅由基础字段组成的复合结构体也是复制类型。字符串、数组、指针和盒子是移动类型。

---

## 表达式

### 字面量

```miva
42            // int
3.14          // float64
true          // bool
false         // bool
'a'           // char
"hello"       // string
"""           // 多行字符串字面量
line one
line two
"""           // （""" 标记之间的内容）
```

### 二元运算符

| 运算符 | 说明 | 操作数类型 |
|--------|------|------------|
| `+` | 加法 / 字符串拼接 | int, float32, float64, string |
| `-` | 减法 | int, float32, float64 |
| `*` | 乘法 | int, float32, float64 |
| `==` | 等于 | 所有可比较类型 |
| `!=` | 不等于 | 所有可比较类型 |

运算符优先级（低到高）：
1. `==`, `!=`
2. `+`, `-`
3. `*`

### 一元运算符

```miva
addr x     // 取地址：返回 ptr<T>
deref p   // 解引用：需要 ptr<T>
```

### 强转表达式

```miva
x as int           // 强转为 int
y as float64       // 强转为 float64
c as char          // 强转为 char
```

有效的强转：
- `int ↔ float32`、`int ↔ float64`、`float32 ↔ float64`
- `int ↔ char`
- `bool → int`
- 同类型强转（恒等）

### If 表达式

```miva
// 无 else 的 if（返回 void/null）
if (condition) {
  do_something();
};

// if-else（两个分支必须具有相同类型）
result := if (condition) {
  10
} else {
  20
};
```

`if` 是一个返回值的表达式。当两个分支都返回值时，它们必须具有相同类型。没有 `else` 分支时，表达式产生 `null`。

### Choose（模式匹配）

```miva
choose (x) {
  when (1) { println("one"); }
  when (2) { println("two"); }
  otherwise { println("other"); }
};
```

- 被匹配变量与 `when` 值的类型必须相同。
- 所有分支必须具有相同类型。
- `otherwise` 是 **必需的** —— 省略会触发编译器错误 E0011。

### 代码块

```miva
// 块表达式 —— 返回最后一个表达式
result := {
  let x int = 10;
  let y int = 20;
  x + y         // ← 块结果
};

// 带显式返回的块
{
  prints("hello");
  prints(" ");
  prints("world");
}     // ← void 块
```

---

## 语句

### Let 语句

```miva
// 类型推断（不可变）
name := value;

// 类型推断（可变）
mut name := value;

// 显式类型
let name Type = value;
```

### 表达式语句

任何后跟 `;` 的表达式都是一条语句：

```miva
println("test");
x + 1;
```

### 返回语句

```miva
return x + 1;
return;       // 返回 void
```

### 赋值语句

```miva
// 变量必须是可变的
x = x + 1;
```

### 空语句

```miva
;   // 空操作
```

---

## 控制流

### If / Elif / Else

```miva
if (condition) {
  ...
} elif (other_condition) {
  ...
} else {
  ...
};
```

注意：作为语句使用时，闭合的 `}` 后需跟 `;`。

### While 循环

```miva
while (condition) {
  ...
};
```

### 无限循环

```miva
loop {
  ...
};
```

### For-In 循环

```miva
for i in (range(10)) {
  printlns!(i);
};
```

for-in 循环遍历一个数组。循环变量的类型是数组的元素类型。

---

## 函数

### 函数语法

```miva
// name = (params): return_type => expression
add = (a: int, b: int): int => a + b

// 多语句函数（块体）
factorial = (n: int): int => {
  if (n <= 1) {
    return 1;
  } else {
    return n * factorial(n - 1);
  };
}
```

### 参数

```miva
// 拥有（own）参数（所有权转移）
foo = (x: int, y: string) => { ... }

// ref 参数（借用的 const 引用）
bar = (ref x: int, ref s: string) => { ... }
```

- **`ref`** 参数在 C++ 中以 `const&` 传递 —— 不发生所有权转移。
- **`own`** 参数（默认）获得所有权；该参数可被移动。

### 调用函数

```miva
// 普通调用
add(3, 4);

// 带显式类型实参的调用（泛型函数）
identity[int](42);
mk_pair[int, string](1, "one");

// 方法调用语法（脱糖为函数调用）
x.twice()                   // → twice(x)
n.add(5)                    // → add(n, 5)
n.add(3).add(4)             // → add(add(n, 3), 4)

// 带类型实参的方法调用
p.first[int, string]()      // → first[int, string](p)
```

方法调用语法会自动将接收者插入为第一个参数。

### 泛型函数

```miva
// 单个类型参数
identity[T] = (x: T): T => x

// 多个类型参数
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// 带显式类型实参的调用
let p Pair[int, string] = mk_pair[int, string](1, "one");
```

类型参数通常可从实参推断：

```miva
let x = identity[int](42);   // 显式
let y = identity(42);        // 推断（若编译器能推导出 T）
```

### 嵌套函数（仅顶层定义）

所有函数定义都是顶层的。块内不支持嵌套函数表达式。

### 递归

函数可以递归（调用自身）。不保证完整的尾调用优化。

---

## 结构体

Miva 支持结构体上的泛型。

### 泛型结构体

```miva
Box[T] = struct {
  value: T,
}

Pair[T, U] = struct {
  first: T,
  second: U,
}

// 空类型参数的结构体
Empty[T] = struct {}
```

### 泛型函数

```miva
// 使用泛型类型的泛型函数
mk_box[T] = (x: T): Box[T] => struct Box[T] { value = x }

// 函数中的多个类型参数
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// 嵌套泛型
mk_nested[T, U] = (a: T, b: U): Box[Pair[T, U]] => struct Box[Pair[T, U]] { value = mk_pair[T, U](a, b) }
```

类型实参使用方括号语法：`func[T, U](args)`。

泛型函数被编译为 C++ 模板。泛型结构体成为 C++ 模板结构体。两者都必须在头文件中完整定义，因此导出的泛型函数以内联形式生成。

---

## 安全级别

Miva 为函数提供三个安全级别：

### 安全（默认）

```miva
// 所有函数默认都是安全的
main = () => {
  println("Hello");
}
```

安全函数 **不能**：
- 调用 `unsafe` 函数
- 解引用指针（`deref` 表达式）
- 使用原始指针内建函数（`ptr_alloc`、`ptr_realloc`、`ptr_free`、`ptr_set`）

### 不安全

```miva
unsafe dangerous_op = (p: ptr<int>) => {
  deref p;
}
```

不安全函数可以：
- 调用其他不安全函数
- 使用 `deref` 和 `addr`
- 使用原始指针内建函数

### 受信任

```miva
trusted safe_wrapper = (p: ptr<int>): int => {
  return deref p;
}
```

受信任函数可执行不安全操作，但可从安全代码调用。它们是围绕不安全原语的安全抽象。

### 安全限制流

```
安全函数 → 可调用：safe、trusted（不可：unsafe）
不安全函数 → 可调用：safe、unsafe、trusted
受信任函数 → 可调用：safe、unsafe、trusted
```

---

## 移动语义与所有权

Miva 使用受 Rust 启发的所有权系统与移动语义。

### 移动

```miva
// 移动转移所有权
main = () => {
  s := "hello";           // s 拥有该字符串
  consume(move s);        // s 被移动；s 变为无效
  printlns!(s);           // 错误：使用了已移动的值 's'（E0001）
}
```

### 克隆

```miva
main = () => {
  s := "hello";
  consume(clone s);       // s 被克隆；s 仍然有效
  printlns!(s);           // 正确
}
```

### 复制类型

基础类型（`int`、`bool`、`float32`、`float64`、`char`）以及完全由复制类型组成的复合结构体自动复制 —— 无需显式 `clone`。

### Ref 参数

`ref` 参数借用（而非移动）值。它们不能被移动：

```miva
bar = (ref x: int) => {
  move x;                // 错误：不能移动 ref 参数（E0002）
}
```

### 移动后赋值

对可变变量赋值会重置其状态，使其再次有效：

```miva
mut x := 42;
consume(move x);         // x 被移动
x = 99;                  // x 再次有效
```

### If/Choose 分支合并

在 `if` 表达式之后，若一个变量在 **所有** 分支中都被移动，则视为在 `if` 之后已被移动。若仅在某一分支中被移动，则仍然有效（因为两个分支都必须移动它）。

---

## 异步（Async）

Miva 提供基于线程的异步模型：用 `async` 关键字声明的函数会在被调用时立即在独立的 OS 线程上启动，并返回一个 `future[T]` 句柄；随后用 `.await()` 或 `await(...)` 阻塞并取回结果。

### 语法

`async` 函数必须在返回类型上标注 `future[T]` —— 其元素类型 `T` 即该任务的结果类型：

```miva
async square = (x: int): future[int] => {
  return x * x;
}
```

调用 `async` 函数不会阻塞：它立刻返回一个 `future[int]`。对该句柄调用 `.await()`（或 `await(handle)`）会等待线程结束并取回内部的 `int`。

### 示例

来自 `examples/async/src/main.miva`：

```miva
module main;

async square = (x: int): future[int] => {
  return x * x;
}

async add = (a: int, b: int): future[int] => {
  return a + b;
}

async greet = (name: string): future[string] => {
  return "hello " + name;
}

async combine = (x: int): future[int] => {
  return add(x, square(x).await()).await();
}

main = () => {
  f := square(5);                       // f 立即是 future[int]，任务已在后台运行
  g := greet("miva");                   // 同上
  a := square(3).await();               // 阻塞直到 square(3) 结束
  b := square(4).await();
  printlns!(f.await(), g.await(), a, b);
  printlns!(combine(7).await());
  printlns!(add(square(2).await(), square(3).await()).await());
}
```

要点：

- 调用 `async` 函数 **立即返回** `future[T]`，任务在后台线程并发执行。
- `.await()` 是方法调用语法糖，脱糖为 `await(...)`，二者等价。
- `.await()` 可链式使用（如 `combine` 内部），用于组合多个异步任务。
- 对 **非** future 的值调用 `await(...)` 是恒等操作 —— 直接返回原值，因此 `await` 可安全包裹任意表达式。

### 类型

`future[T]` 是内建复合类型。`async` 函数的声明返回类型必须形如 `future[T]`，否则类型检查会报错（"async function must return future[T]"）。类型实参 `T` 可以是任意 Miva 类型，包括 `string`、结构体等。

| 类型 | 说明 | C++ 映射 |
|------|------|-----------|
| `future[T]` | T 任务的句柄 | `mvp_future<T>` |

### 各后端实现

- **C++（`cxx`）** —— `async` 函数被编译为返回 `mvp_future<T>` 的包装函数；函数体被捕获进一个 lambda，经 `mvp_async_spawn` 以 `std::async(std::launch::async)` 在 `std::future` 上运行。`.await()` 映射为 `mvp_async_await`，调用 `std::future::get()` 取回结果。`shared_ptr` 使 future 可复制，因此 `let f = task(); f.await()` 与 `task().await()` 都合法。
- **LLVM（`llvm`）** —— 调用 `async` 函数会通过运行时桥 `miva_async_spawn`（基于 `std::thread` 的结构体）生成一个独立 OS 线程，返回任务句柄（i64）；`await(...)` 调用 `miva_async_await`，通过 `std::condition_variable` 等待任务完成并 join 线程。
- **MVM（`mvm`）** —— 字节码 `Call` 指令在目标是 `async` 函数时，把实参收集到新线程、启动一个独立的 `Mvm` 实例运行该函数，并压入一个 `Value::Future`（持有结果与线程句柄）。`await` 字节码（`Opcode::Await`）会 join 该线程并取回结果。

### 安全与并发语义

- `async` 函数默认是 **safe** 级别，可调用其他 safe / trusted 函数，并受移动/所有权规则约束。其参数按值捕获进后台线程（包括 `ref` 参数，会被复制以避免悬垂引用）。
- 异步任务与调用方并发运行于不同线程，共享不可变数据需自行保证；Miva 当前不提供语言内建的锁原语，互斥由标准库或 `inline unsafe` 的 C/C++ 代码承担。
- `await` 会阻塞当前线程直到该 future 完成，因此多次 `await` 同一个句柄是安全且幂等的。

---

## 方法调用语法糖

方法调用语法 `receiver.method(args...)` 会在编译时自动脱糖为 `method(receiver, args...)`。

```miva
twice = (x: int): int => x * 2
add = (a: int, b: int): int => a + b

main = () => {
  n := 10;

  // 无额外参数：n.twice() → twice(n)
  prints(n.twice())

  // 一个额外参数：n.add(5) → add(n, 5)
  prints(n.add(5))

  // 链式：n.twice().add(5) → add(twice(n), 5)
  prints(n.twice().add(5))

  // 带嵌套方法调用的链式：
  // n.add(3).add(n.add(4)) → add(add(n, 3), add(n, 4))
  prints(n.add(3).add(n.add(4)))
}
```

方法调用语法支持泛型类型实参：

```miva
p.first[int, string]()          // → first[int, string](p)
```

---

## 宏

Miva 有两种宏：**内建宏** 和 **用户自定义宏**。

### 内建宏

#### `prints!(...)`

打印多个值，以空格分隔。自动将值转换为字符串。

```miva
prints!("hello", 42, true);    // 输出："hello 42 true "
```

展开为：

```miva
let s string = "";
s = s + string_from("hello") + " ";
s = s + string_from(42) + " ";
s = s + string_from(true) + " ";
print(s);
```

#### `printlns!(...)`

与 `prints!` 相同，但追加末尾换行。

```miva
printlns!(1, 2, 3);    // 输出："1 2 3\n"
```

#### `assert!(expr)`

若表达式求值为 `false`，则以 "Assertion failed" 触发 panic。

```miva
assert!(x == 42);
```

展开为：

```miva
if (x == 42 == false) {
  panic("Assertion failed");
} else {};
```

#### `include_str!("path")`

在编译期读取文件，并将其内容作为字符串字面量嵌入。

```miva
let contents string = include_str!("data.txt");
```

### 用户自定义宏

```miva
// 宏定义
macro double = ($x: int) => $x + $x

macro greet = ($name: string) => {
  prints!("Hello, ");
  prints!($name);
  prints!("!\n");
}

// 宏调用
double!(5);        // 展开为：5 + 5
greet!("World");   // 展开为问候代码块
```

宏语法：
- 参数以 `$` 为前缀：`$name`、`$x` 等。
- 参数具有显式类型：`($x: int, $y: string)`
- 宏体使用 `=>` 语法，与函数相同
- 在宏体内，`$param` 引用会变成 `EMacroVar` 节点，在展开时用实参表达式替换

宏在语义分析和类型检查 **之前** 展开。这意味着宏可处理任何表达式类型，且错误报告针对展开后的代码。

### 宏作用域

宏在项目范围内 **先于** 编译收集。项目中任一文件定义的宏对所有其他文件可用。`DMacro` 定义在展开后从 AST 中移除。

### 嵌套宏

宏可以调用其他宏（包括嵌套的内建宏）：

```miva
macro assert_eq = ($got: int, $expected: int) => {
  if ($got != $expected) {
    prints!("FAIL: expected ");
    printlns!($expected);
  } else {};
}
```

---

## 内建函数

### 输出函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `print` | `(s: string)` | 打印字符串 |
| `prints` | `(s: string)` | 打印字符串（已弃用，请使用 `prints!`） |
| `println` | `(s: string)` | 打印字符串并换行 |
| `printlns` | `(s: string)` | 打印字符串并换行（已弃用，请使用 `printlns!`） |
| `error` | `(s: string)` | 打印到 stderr |
| `errors` | `(s: string)` | 打印到 stderr（已弃用） |
| `errorln` | `(s: string)` | 打印到 stderr 并换行 |
| `errorlns` | `(s: string)` | 打印到 stderr 并换行（已弃用） |

### 控制函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `exit` | `(code: int)` | 以指定退出码退出进程 |
| `abort` | `()` | 中止进程 |
| `panic` | `(msg: string)` | 以消息触发 panic（带消息中止） |

### 字符串函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `string_concat` | `(a: string, b: string): string` | 拼接字符串（已弃用） |
| `string_parse` | `(s: string): int` | 将字符串解析为整数（已弃用） |
| `string_length` | `(s: string): int` | 获取字符串长度（已弃用） |
| `string_make` | `(s: string, n: int): string` | 生成字符串（已弃用） |
| `string_from` | `(x: T): string` | 将值转换为字符串 |

### 盒子函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `box_new` | `(x: T): box<T>` | 创建一个新盒子 |
| `box_deref` | `(b: box<T>): T` | 解引用一个盒子 |

### Range 函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `range` | `(n: int): array<int>` | 创建数组 `[0, 1, ..., n-1]` |

### 不安全指针函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `ptr_alloc` | `(size: int): ptrany` | 分配内存 |
| `ptr_realloc` | `(p: ptrany, size: int): ptrany` | 重新分配内存 |
| `ptr_free` | `(p: ptrany)` | 释放内存 |
| `ptr_set` | `(p: ptrany, v: int)` | 写入指针 |

所有指针函数都是 **不安全的**，只能在 `unsafe` 或 `trusted` 函数中使用。

### FFI（外部函数接口）

以 `ffi.` 为前缀的函数映射到 C++ 命名空间调用：

```miva
ffi.some_c_func(a, b);    // 编译为：some_c_func(a, b)
ffi.ns.func(args);        // 编译为：ns::func(args)
```

没有自动的 C 绑定生成；C 函数必须手动链接或通过 `c unsafe` 提供。

---

## 标准库

Miva 标准库（`std-0.1.2`）提供：

### `std.str` —— 字符串工具

```miva
import "std/str";

std.str.concat(ref a, ref b)      // 字符串拼接
std.str.parse_int(ref s)           // 字符串转整数解析
std.str.len(ref s)                 // 字符串长度
std.str.make(ref s, ref size)     // 字符串重复
std.str.from[T](x)                // 值转字符串（泛型）
```

`from[T]` 的类型参数被导出，使其可跨模块使用。

### `std.io` —— 带颜色 I/O

```miva
import "std/io";

std.io.cprint(ref x, ref color)     // 带颜色打印
std.io.cprintln(ref x, ref color)   // 带颜色打印行
std.io.eprint(ref x, ref color)     // 带颜色错误打印
std.io.eprintln(ref x, ref color)   // 带颜色错误打印行
```

### `std.mem` —— 内存管理

```miva
import "std/mem";

std.mem.alloc(ref size)             // 分配（ptrany）
std.mem.realloc(ref p, size)        // 重新分配
std.mem.free(ref p)                 // 释放
```

### `std.term` —— 终端颜色码

```miva
import "std/term";

std.term.color_null()     // 重置颜色
std.term.color_black()    // "\x1b[0;30m"
std.term.color_red()      // "\x1b[0;31m"
std.term.color_green()    // "\x1b[0;32m"
std.term.color_yellow()   // "\x1b[0;33m"
std.term.color_blue()     // "\x1b[0;34m"
std.term.color_magenta()  // "\x1b[0;35m"
std.term.color_cyan()     // "\x1b[0;36m"
std.term.color_white()    // "\x1b[0;37m"
```

### `std.vec` —— 向量

```miva
import "std/vec";

std.vec.new[T]()                  // 创建空向量
std.vec.push[T](ref v, x)         // 追加元素
std.vec.get[T](ref v, i)          // 按索引获取元素
std.vec.len[T](ref v)             // 元素数量
std.vec.pop[T](ref v)             // 移除并返回最后一个元素
```

### `std.box` —— 盒装值

```miva
import "std/box";

std.box.new[T](x)                 // 创建盒装值
std.box.get[T](ref b)             // 解引用盒子
std.box.set[T](ref b, x)          // 更新盒装值
```

### `std.json` —— JSON 解析

```miva
import "std/json";

std.json.parse(ref s)             // 解析字符串 -> ptrany（JSON 树）
std.json.object_get(ref v, i)     // 按索引获取对象/数组元素
std.json.object_len(ref v)        // 键/元素数量
std.json.object_key(ref v, i)     // 按索引获取对象键
std.json.object_find(ref v, k)    // 按键查找对象值
std.json.kind(ref v)              // JSON 节点类型（bool/number/string/array/object）
std.json.bool(ref v)              // 提取布尔值
std.json.number(ref v)            // 提取数值
std.json.string(ref v)            // 提取字符串值
std.json.stringify(ref v)         // 将 JSON 树序列化为字符串
std.json.free(ref v)              // 释放 JSON 树
```

### `std.xml` —— XML 解析

```miva
import "std/xml";

std.xml.parse(ref s)              // 解析字符串 -> ptrany（XML 树）
std.xml.kind(ref v)               // 节点类型（element/text/comment/cdata/pi）
std.xml.tag(ref v)                // 元素标签名
std.xml.attr_count(ref v)         // 属性数量
std.xml.attr_name(ref v, i)       // 按索引获取属性名
std.xml.attr_value(ref v, i)      // 按索引获取属性值
std.xml.attr_find(ref v, k)       // 按名称查找属性值
std.xml.child_count(ref v)        // 子节点数量
std.xml.child_get(ref v, i)       // 按索引获取子节点
std.xml.text(ref v)               // 文本节点的文本内容
std.xml.comment(ref v)            // 注释内容
std.xml.cdata(ref v)              // CDATA 内容
std.xml.pi_target(ref v)          // 处理指令目标
std.xml.pi_data(ref v)            // 处理指令数据
std.xml.stringify(ref v)          // 将 XML 树序列化为字符串
std.xml.free(ref v)               // 释放 XML 树
```

### `std.toml` / `std.yaml` —— TOML 与 YAML 解析

两者都暴露与 JSON 相同的树形 API（parse → `ptrany`，然后 `object_get`/`object_len` 等），并复用 JSON 节点表示。

```miva
import "std/toml";
import "std/yaml";

std.toml.parse(ref s)             // 解析 TOML 字符串 -> ptrany
std.yaml.parse(ref s)             // 解析 YAML 字符串 -> ptrany
```

---

## C FFI（外部函数接口）

Miva 允许通过 `inline unsafe` 函数语法嵌入原始 C++ 代码：

```miva
// 使用 "inline" 关键字（推荐）
inline unsafe printf_wrapper = (fmt: string): int => {
  return printf("%s", fmt);
}

// 使用 "c" 关键字（已弃用，会触发 W0004 警告）
c unsafe puts = (s: string): int => {
  return puts(s);
}
```

`{ }` 之间的 C++ 代码会直接插入到生成的 C++ 翻译单元中。在 `llvm` 和 `mvm` 后端上，`inline`/`c` 原始代码块不可用，此类函数必须由外部提供或省略。

### 无花括号的原始 C 函数（字符串体）

```miva
inline unsafe custom_fn = (x: int): int => "return x * 2;"
```

这避免了花括号包裹的原始代码块，使用字符串字面量作为函数体。

---

## 运算符重载

运算符重载通过 `impl` 块支持：

```miva
impl Point {
  op_add my_add,    // my_add(a, b) → a + b
  op_sub my_sub,    // my_sub(a, b) → a - b
  op_mul my_mul,    // my_mul(a, b) → a * b
  op_eq my_eq,      // my_eq(a, b) → a == b
  op_neq my_neq,    // my_neq(a, b) → a != b
}
```

这会为结构体类型生成 C++ 的 `operator+`、`operator-`、`operator*`、`operator==` 和 `operator!=` 函数，并委托给命名函数。

---

## 编译器流程与命令

### 流程

```
Source (.miva)
  ↓ 词法 & 语法分析（miva-frontend-rs）
JSON AST
  ↓ 项目范围内收集宏
  ↓ 宏展开（内建 + 用户自定义）
  ↓ 符号表构建
  ↓ 语义分析
    • 变量解析
    • 移动语义
    • 安全强制执行
    • 模块/导入校验
  ↓ 类型检查
    • 类型推断
    • 泛型类型替换
    • 类型一致性验证
  ↓ 警告生成与过滤
  ↓ 代码生成（C++ / LLVM IR / MVM 字节码）
C++ 源文件 (.cpp, .h)      LLVM IR (.ll)          MVM 字节码 (.mvm)
  ↓ g++ (C++20)               ↓ llc + g++ 链接器         ↓ mvm 解释器
  ↓ 目标文件 (.o)                                     （无需原生链接器）
  ↓ 链接
原生二进制 (.exe / .so)
```

### 命令

| 命令 | 说明 |
|------|------|
| `miva init <name> -t <bin\|lib>` | 初始化新项目 |
| `miva reinit` | 从模板重新生成 `miva.toml` 并移除 `miva.lock` |
| `miva build [-b <cxx\|llvm\|mvm>]` | 构建项目 |
| `miva run [-b <cxx\|llvm\|mvm>]` | 构建并运行（`-b mvm` / `--mvm` 在解释器上运行） |
| `miva clean` | 清理构建产物 |
| `miva sin-build <file>` | 编译单个文件 |
| `miva sin-run <file>` | 编译并运行单个文件 |
| `miva test <file>` | 运行测试文件 |
| `miva get <url>` | 安装依赖 |
| `miva dep` | 显示从 `main.miva` 开始的依赖图 |
| `miva <script>` | 运行 `[scripts]` 中定义的自定义脚本 |

选项：
- `--release` —— Release 模式（优化，`-O2`）
- `--verbose` / `-v` —— 详细输出
- `-b <backend>` / `--backend <backend>` —— 后端：`cxx`（默认）、`llvm` 或 `mvm`
- `--mvm` —— 等价于 `-b mvm`；生成字节码并在 MVM 解释器上运行

### 输出结构

```
build/
├── debug/
│   ├── <project_name>      # 调试原生可执行文件（cxx/llvm）
│   └── <project_name>.mvm  # 调试字节码（mvm 后端）
└── release/
    ├── <project_name>
    └── <project_name>.mvm

build/debug/cache/
├── src/
│   ├── main.miva.cpp       # 生成的 C++ 源文件（cxx 后端）
│   ├── main.miva.h         # 生成的 C++ 头文件（导出）
│   ├── main.miva.ll        # 生成的 LLVM IR（llvm 后端）
│   ├── main.miva.mvm       # 生成的字节码（mvm 后端）
│   ├── main.miva.o         # 编译目标文件
│   └── main.miva.sha256    # 用于缓存的源哈希
├── std/src/
│   ├── str.miva.cpp
│   ├── str.miva.o
│   └── ...
└── ...
```

---

## 错误码

### 语义错误

| 代码 | 说明 |
|------|------|
| E0001 | 使用了已移动的值 |
| E0002 | 不能移动 ref 参数 / 不能对不可变变量赋值 |
| E0004 | 重复的函数或结构体定义 |
| E0005 | 模块声明必须在顶部 / 只能有一个模块 / 重复模块 |
| E0007 | 未找到变量 |
| E0009 | 不能从安全函数调用不安全函数 / 未知函数 |
| E0010 | 不能在安全函数中解引用指针 |
| E0011 | Choose 表达式必须含有 otherwise 分支 |
| E0013 | 无效的 Magical 注释 |

### 类型错误

| 代码 | 说明 |
|------|------|
| E0014 | 类型不匹配 / 需要非空值处使用了 void 值 |
| E0016 | 函数实参数量或类型不匹配 |
| E0017 | 返回类型不匹配 |
| E0018 | 结构体字面量错误（未知结构体 / 字段类型错误 / 缺失字段） |
| E0019 | 结构体中未知字段 |
| E0021 | 无效的强转 |
| E0022 | let 声明或赋值中的类型不匹配 |
| E0024 | 所有数组元素必须具有相同类型 |
| E0026 | For-each 循环范围必须是数组 |

---

## 警告码

| 代码 | 说明 |
|------|------|
| W0001 | 命名规范违反（非 snake_case 的函数/变量、非小写的模块） |
| W0002 | 弃用函数使用（请改用标准库替代） |
| W0003 | 无效的 Intro 注释注解 |
| W0004 | 弃用关键字使用（`c` → 改用 `inline`） |

警告可通过 Magical 指令控制：

```miva
/! warning_off W0001    // 抑制命名警告
/! warning_err W0002    // 将弃用警告视为错误
```

---

## 弃用函数

| 函数 | 替代 |
|------|------|
| `prints` | 宏 `prints!` |
| `printlns` | 宏 `printlns!` |
| `string_concat` | `std.str.concat` |
| `string_parse` | `std.str.parse_int` |
| `string_length` | `std.str.len` |
| `string_make` | `std.str.make` |
| `ptr_alloc` | `std.mem.alloc` |
| `ptr_realloc` | `std.mem.realloc` |
| `ptr_free` | `std.mem.free` |

## 词汇表

- **AST** —— 抽象语法树，源码结构的内部表示
- **FFI** —— 外部函数接口，调用 C/C++ 函数的机制
- **Move** —— 值的所有权转移，使源失效
- **Clone** —— 值的显式复制，源仍然有效
- **Ref** —— 对值的借用引用（const 引用传递）
- **Own** —— 拥有的参数（按值传递，所有权转移）
- **Box** —— 堆分配的值，具有自动生命周期管理
- **Magical** —— 控制警告、release 模式等的编译器指令
- **Intro** —— 标注下一个定义安全性/用途的注解注释
- **Sir (sin-)** —— 单文件编译（无需项目配置）
