# Miva 编程语言

Miva 是一种编译型系统编程语言。其编译器将 Miva 源码转译为 C++（默认后端），或转译为 LLVM IR，或转译为 Miva 虚拟机（MVM）字节码，并编译为原生二进制文件，或直接由解释器运行。它具备强静态类型系统、泛型编程、代数数据类型（枚举）、模式匹配、闭包、移动语义、安全系统、宏，以及零开销的 C FFI。

## 目录

- [概览](#概览)
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
- [闭包](#闭包)
- [结构体](#结构体)
- [枚举](#枚举)
- [泛型](#泛型)
- [安全级别](#安全级别)
- [移动语义与所有权](#移动语义与所有权)
- [异步](#异步)
- [方法调用语法糖](#方法调用语法糖)
- [宏](#宏)
- [内置函数](#内置函数)
- [标准库](#标准库)
- [C FFI（外部函数接口）](#c-ffi外部函数接口)
- [运算符重载](#运算符重载)
- [编译器流程与命令](#编译器流程与命令)
- [错误码](#错误码)
- [警告码](#警告码)

---

## 概览

Miva 是一个全栈式编程语言编译器：

1. **前端**（`miva-frontend-rs`）——对 `.miva` 源文件进行词法与语法分析，生成 JSON AST。
2. **编译器**（`miva`）——加载 JSON AST，执行宏展开、语义分析、类型检查，并为所选后端生成代码。
3. **后端**——三种后端之一（见下文）将生成产物转化为原生二进制文件，或在 MVM 解释器上运行。

编译流程：

```
.miva 源码 → 词法/语法分析 → JSON AST
  → 宏展开 → 符号表 → 语义分析
  → 类型检查 → 代码生成（C++ / LLVM IR / MVM 字节码）→ 原生二进制 / MVM
```

### 后端

Miva 支持三种后端，可通过 `-b` 标志或 `miva.toml` 中的 `[project] backend` 字段按构建选择：

| 后端 | `-b` 取值 | 输出 | 说明 |
|---------|------------|--------|-------|
| **C++**（`cxx`） | `cxx` / `c++` / `cpp` | 原生可执行文件 / `.so` | 默认。生成由 `g++` 编译的 C++20 代码。 |
| **LLVM**（`llvm`） | `llvm` / `ll` | 原生可执行文件 / `.so` | 生成由 `llc` + `g++` 链接器编译的 LLVM IR。 |
| **MVM**（`mvm`） | `mvm` | `.mvm` 字节码 | 生成 Miva 虚拟机字节码，由 `mvm` 解释器运行（无需原生链接器）。 |

`cxx` 与 `llvm` 后端均生成原生二进制文件。`mvm` 后端生成由内置 `mvm` 解释器（`miva-vm`）执行的便携字节码，适用于快速迭代与跨平台运行。

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
    └── main.miva  # 入口点（库项目为 src/lib.miva）
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

每个 Miva 项目必须在根目录包含一个 `miva.toml`：

```toml
[project]
name = "myapp"
type = "bin"         # "bin" 表示可执行文件，"lib" 表示共享库
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

- **`bin`** —— 编译为带有 `main()` 入口点的原生可执行文件（使用 `src/main.miva`）。
- **`lib`** —— 编译为共享库（`.so`），带 `-fPIC` 与 `-shared` 标志。使用 `src/lib.miva` 作为入口。

### 后端选择

后端按如下优先级选择：命令行 `-b` / `--mvm` 标志，其次 `miva.toml` 中的 `[project] backend` 字段（默认 `cxx`）。后端细节见 [编译器流程与命令](#编译器流程与命令)。

### 脚本

`[scripts]` 段定义可作为 `miva <name>` 运行的自定义命令。内置命令名（`init`、`build`、`run`、`clean`、`sin-build`、`sin-run`、`get`、`dep`、`test`、`reinit`）始终优先于脚本。

### 依赖

依赖从标准库路径获取。标准库以 `std-0.1.2` 形式内置提供：

```toml
[dependencies]
std = "0.1.2"
```

来自 GitHub 的依赖可通过以下方式安装：

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

/! 魔法指令（控制编译器行为）
```

### 魔法指令

```miva
/! warning_off W0001    // 抑制警告 W0001
/! warning_err W0002    // 将警告 W0002 视为错误
/! release always       // 标记为仅发布版本可用
/! mangle name          // 自定义名称修饰（name mangling）
```

### 引导注释（注解）

```miva
@ unsafe: 执行原始内存操作
@ usage: 用作内部辅助函数
@ param: x 为输入值
@ impl: 结构体的 trait 实现
@ trusted: 不安全代码的安全封装
```

引导注释对紧随其后的定义进行标注，并会进行正确性校验（例如 `unsafe` 注解仅可用于 `unsafe` 函数之前，`usage` 可用于任何定义之前，等等）。

---

## 模块系统

每个 Miva 文件必须恰好声明一个模块：

```miva
module main;          // 简单模块
module std.io;        // 带命名空间的模块（在 C++ 中生成 mvp_std::io）
module my.app.utils;  // 深层命名空间
```

模块声明**必须**出现在文件顶部，位于任何其他定义之前。

### 导入

```miva
// 基础导入
import "std/str";

// 带命名空间别名的导入
import "std/io" as io;

// 导入并引入当前命名空间
import "std/io" as .;

// 导入 C 头文件（生成 #include <stdio.h>）
import "c:stdio.h";
```

导入解析规则：
- `proj_name/path` —— 项目内部：解析为 `src/path.miva`
- `std/path` —— 标准库：解析为标准库包含目录
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

// 带参数与返回类型的函数
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

// 带引用（借用）参数的函数
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

### 枚举

```miva
// 简单枚举
Shape = enum {
  Circle(int),
  Rect(int, int),
  Empty
}

// 泛型枚举
Option[T] = enum {
  Some(T),
  None
}

Result[T, E] = enum {
  Ok(T),
  Err(E)
}
```

枚举是代数数据类型（带标签的联合体）。每个变体可携带零个或多个值的负载。枚举类型使用 `choose`/`when` 进行匹配（见 [控制流](#控制流)）。

### 结构体字面量

```miva
let p Point = struct Point { x = 10, y = 20 };
let b Box[int] = struct Box[int] { value = 42 };
```

### 枚举构造器

枚举通过调用变体名构造：

```miva
let circle Shape = Shape.Circle(5);
let rect Shape = Shape.Rect(3, 4);
let empty Shape = Shape.Empty;

// 带显式类型参数（用于泛型枚举）
let some_val Box[int] = Box.Value(42);
let no_val Box[int] = Box.Empty;
let s Box[string] = Box[string].Value("hello");

// 由参数推断类型
let inferred Box[int] = Box.Value(7);

// 多个类型参数
let p Pair[int, string] = Pair.Both(1, "one");
let q Pair[int, string] = Pair[int, string].First(99);
```

枚举变体名作为函数调用。对于泛型枚举，可在变体调用上指定类型参数（如 `Box[int].Value(42)`），或由负载推断。

### 枚举字段访问

枚举变体负载值可按位置访问：

```miva
payload_value = s.0;  // 第一个负载字段
```

这适用于 `choose`/`when` 解构，或直接用于枚举值。

### 测试

```miva
test test_name = (): int => {
  assert!(some_condition);
  0;
}
```

测试作为测试可执行文件单独编译。它们必须返回 `int`。

---

## 类型

### 基础类型

| 类型 | 描述 | C++ 映射 |
|------|-------------|-------------|
| `int` | 有符号 64 位整数 | `mvp_builtin_int`（int64_t） |
| `bool` | 布尔值 | `mvp_builtin_boolean` |
| `float32` | 32 位浮点数 | `mvp_builtin_float` |
| `float64` | 64 位浮点数 | `mvp_builtin_float` |
| `char` | 字符（字节） | `mvp_builtin_byte` |
| `string` | 字符串 | `mvp_builtin_string` |

### 复合类型

| 类型 | 描述 | C++ 映射 |
|------|-------------|-------------|
| `array<T>` | T 的数组/向量 | `std::vector<T>` |
| `ptr<T>` | 指向 T 的指针 | `T*` |
| `box<T>` | 堆分配的 T 盒子 | `mvp_builtin_box<T>` |
| `future[T]` | T 异步任务句柄 | `mvp_future<T>` |
| `ptrany` | 空指针 | `mvp_builtin_ptrany` |
| `null` | 空/无值 | `void` |
| `fn(T1, T2): R` | 函数类型 | 函数指针 / 闭包 thunk |

### 结构体类型

结构体类型通过其名称引用，可附带泛型类型参数：

```miva
let p Point;
let b Box[int];
let pair Pair[int, string];
```

### 枚举类型

枚举类型与结构体类似，通过名称引用：

```miva
let shape Shape;
let opt Option[int];
let res Result[int, string];
```

### 函数类型

函数类型使用 `fn(T1, T2): R` 语法表示闭包与高阶函数：

```miva
f: fn(int): int              // 接收 int，返回 int 的函数
g: fn(int, string): bool     // 接收 int 与 string，返回 bool 的函数
h: fn(): null                // 不接收参数，返回 void 的函数
```

这些用于 lambda 表达式（见 [闭包](#闭包)）。

---

## 变量

### 类型推断变量

```miva
// 类型推断的不可变变量
x := 42;

// 类型推断的可变变量
mut count := 0;

// 类型推断的变量默认不可变
```

### 显式类型变量

```miva
let x int = 42;
let name string = "Miva";
let p Point = struct Point { x = 1, y = 2 };
let s Shape = Shape.Circle(5);
```

### 赋值

```miva
mut x := 10;
x = 20;     // 正确：x 可变

// 错误：无法对不可变变量赋值
y := 10;
y = 20;     // 编译错误
```

### 字段赋值

```miva
mut p := struct Point { x = 1, y = 2 };
p.x = 10;   // 字段赋值
```

### 移动与克隆

```miva
// 移动所有权
move x;                // x 被移动，之后无法使用

// 克隆（复制）值
clone x;               // x 仍然有效
```

基础类型（int、bool、float32、float64、char）为复制类型，无需显式 `clone`。仅含基础字段的结构体也是复制类型。字符串、数组、指针与盒子为移动类型。

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
"""           // 多行字符串字面量（原始字符串，不处理转义）
line one
line two
"""           // （""" 标记之间的内容）
[v1, v2, v3]  // 数组字面量
```

### 二元运算符

| 运算符 | 描述 | 操作数类型 |
|----------|-------------|---------------|
| `+` | 加法 / 字符串拼接 | int、float32、float64、string |
| `-` | 减法 | int、float32、float64 |
| `*` | 乘法 | int、float32、float64 |
| `/` | 除法 | int、float32、float64 |
| `==` | 相等 | 所有可比较类型 |
| `!=` | 不等 | 所有可比较类型 |
| `<` | 小于 | int、float32、float64 |
| `>` | 大于 | int、float32、float64 |
| `<=` | 小于等于 | int、float32、float64 |
| `>=` | 大于等于 | int、float32、float64 |
| `&&` | 逻辑与 | bool |
| `\|\|` | 逻辑或 | bool |

运算符优先级（由低到高）：
1. `||`
2. `&&`
3. `==` `!=` `<` `>` `<=` `>=`
4. `+` `-`
5. `*` `/`

所有二元运算符均为左结合。

### 一元运算符

```miva
addr x     // 取地址：返回 ptr<T>
deref p   // 解引用：要求 ptr<T>
```

### 强制转换表达式

```miva
x as int           // 转换为 int
y as float64       // 转换为 float64
c as char          // 转换为 char
```

合法转换：
- `int ↔ float32`、`int ↔ float64`、`float32 ↔ float64`
- `int ↔ char`
- `bool → int`
- 同类型转换（恒等）

### If 表达式

```miva
// 无 else 的 if（返回 void/null）
if (condition) {
  do_something();
};

// if-else（两个分支必须有相同类型）
result := if (condition) {
  10
} else {
  20
};
```

`if` 是一个返回值的表达式。当两个分支都返回值时，它们必须具有相同类型。没有 `else` 分支时，表达式产生 `null`。

### Choose（模式匹配）

```miva
// 简单值匹配
choose (x) {
  when (1) { println("one"); }
  when (2) { println("two"); }
  otherwise { println("other"); }
};

// 枚举模式匹配
choose (shape) {
  when (Shape.Circle(r)) { return r * r; }
  when (Shape.Rect(w, h)) { return w + h; }
  otherwise { return 0; }
};

// 泛型枚举模式匹配
choose (opt) {
  when (Option.Some(v)) { process(v); }
  when (Option.None) { handle_missing(); }
};

// 带守卫的模式匹配
choose (opt) {
  when (Option.Some(n)) if (n > 0)  { return n; }
  when (Option.Some(n)) if (n == 0) { return 0; }
  when (Option.Some(n))            { return n * -1; }
  otherwise { return -1; }
};

// 不捕获负载的枚举解构（仅类型检查）
choose (opt) {
  when (Option.Some) { return true; }
  otherwise { return false; }
};
```

- 被匹配变量与 `when` 值必须具有相同类型。
- 所有分支必须具有相同类型。
- `otherwise` **必须**存在 —— 若省略则报编译器错误 E0011。
- 枚举模式将变体负载解构为具名变量（如 `r`、`w`、`h`）。
- 守卫（`when (Pattern) if (cond)`）为模式增加额外条件。
- 枚举变体可不绑定负载进行匹配（如 `when (Option.Some)`）。

### 块

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
// 变量必须可变
x = x + 1;

// 字段赋值
target.field = expr;
target.field := expr;
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

注意：作为语句使用时，结束的 `}` 后需跟 `;`。

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

for-in 循环遍历一个数组。循环变量的类型为数组的元素类型。

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

// 单表达式函数（无花括号）
double = (x: int): int => x * 2
```

### 函数安全性

```miva
// 安全函数（默认）
safe_func = () => { ... }

// 不安全函数
unsafe unsafe_func = () => { ... }

// 可信函数
trusted trusted_func = () => { ... }
```

### 异步函数

```miva
async async_func = (x: int): future[int] => {
  return x * x;
}
```

更多细节见 [异步](#异步)。

### 参数

```miva
// 拥有（own）参数（所有权转移）
foo = (x: int, y: string) => { ... }

// 引用（ref）参数（借用，const 引用）
bar = (ref x: int, ref s: string) => { ... }
```

- **`ref`** 参数在 C++ 中以 `const&` 传递 —— 不转移所有权。
- **`own`** 参数（默认）接收所有权；该参数可被移动。

### 调用函数

```miva
// 常规调用
add(3, 4);

// 带显式类型参数的调用（泛型函数）
identity[int](42);
mk_pair[int, string](1, "one");

// 方法调用语法（脱糖为函数调用）
x.twice()                   // → twice(x)
n.add(5)                    // → add(n, 5)
n.add(3).add(4)             // → add(add(n, 3), 4)

// 带类型参数的方法调用
p.first[int, string]()      // → first[int, string](p)
```

方法调用语法自动将接收者插入为第一个参数。

### 泛型函数

```miva
// 单个类型参数
identity[T] = (x: T): T => x

// 多个类型参数
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// 带显式类型参数的调用
let p Pair[int, string] = mk_pair[int, string](1, "one");
```

类型参数通常可由参数推断：

```miva
let x = identity[int](42);   // 显式
let y = identity(42);        // 推断（若编译器可推导 T）
```

### 递归

函数可以递归（调用自身）。不保证完整的尾调用优化。

---

## 闭包

Miva 支持带有捕获与函数类型的 lambda 表达式（匿名函数）。

### Lambda 语法

```miva
// 带块体的 lambda
add_one = (x: int): int => {
  return x + 1;
};

// 单表达式 lambda
double = (x: int): int => x * 2;
```

### 带捕获的 Lambda

Lambda 可捕获来自外围作用域的变量：

```miva
main = () => {
  y := 13;
  add := (x: int): int => { return x + y; };
  printlns!(add(2));     // 15
  printlns!(add(29));    // 42
};
```

### 函数类型

Lambda 类型用 `fn(T1, T2): R` 表示：

```miva
apply = (f: fn(int): int, x: int): int => { return f(x); }

main = () => {
  y := 13;
  add := (x: int): int => { return x + y; };
  printlns!(apply(add, 15));  // 28
};
```

### 闭包编译

- 带捕获的闭包编译为堆分配的 thunk，将捕获的变量与函数指针一并存储。
- 在 MVM 后端上，`MakeClosure` 与 `CallClosure` 字节码处理闭包的创建与调用。
- 在 C++ 后端上，闭包使用生成带捕获列表 C++ lambda 的 lambda 表达式。

---

## 结构体

### 结构体定义

```miva
Point = struct {
  x: int,
  y: int,
}
```

### 泛型结构体

```miva
Box[T] = struct {
  value: T,
}

Pair[T, U] = struct {
  first: T,
  second: U,
}
```

### 结构体字段访问

```miva
let p Point = struct Point { x = 10, y = 20 };
printlns!(p.x);
```

### 字段赋值

```miva
mut p := struct Point { x = 1, y = 2 };
p.x = 42;
```

---

## 枚举

枚举是代数数据类型（带标签的联合体），可表示若干变体之一，每个变体可选携带负载。

### 枚举定义

```miva
// 带负载变体的简单枚举
Shape = enum {
  Circle(int),
  Rect(int, int),
  Empty
}

// 泛型枚举
Option[T] = enum {
  Some(T),
  None
}

Result[T, E] = enum {
  Ok(T),
  Err(E)
}
```

### 枚举构造

枚举值通过以函数形式调用变体名构造：

```miva
let c Shape = Shape.Circle(5);
let r Shape = Shape.Rect(3, 4);
let e Shape = Shape.Empty;
```

对于泛型枚举，类型参数可指定或推断：

```miva
// 构造器上的显式类型参数
let b Box[string] = Box[string].Value("hello");

// 由负载推断类型
let b2 Box[int] = Box.Value(42);

// 多个类型参数
let p Pair[int, string] = Pair.Both(1, "one");
```

### 枚举解构

枚举使用 `choose`/`when` 模式解构：

```miva
area = (s: Shape): int => choose (s) {
  when (Shape.Circle(r)) { return r * r; }
  when (Shape.Rect(w, h)) { return w + h; }
  otherwise { return 0; }
}
```

### 枚举守卫

模式可用 `if` 守卫细化：

```miva
describe = (opt: Option[int]): int => choose (opt) {
  when (Option.Some(n)) if (n > 0)  { return n; }
  when (Option.Some(n)) if (n == 0) { return 0; }
  when (Option.Some(n))            { return n * -1; }
  otherwise { return -1; }
}
```

### 不绑定负载的枚举

变体可不绑定负载值进行匹配：

```miva
is_some[T] = (ref o: Option[T]): bool => choose (o) {
  when (Option.Some) { return true; }
  otherwise { return false; }
}
```

---

## 泛型

Miva 在结构体、枚举与函数上支持泛型。

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

### 泛型枚举

```miva
Option[T] = enum {
  Some(T),
  None
}

Pair[A, B] = enum {
  Both(A, B),
  First(A),
  None
}
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

类型参数使用方括号语法：`func[T, U](args)`。

泛型函数编译为 C++ 模板。泛型结构体变为 C++ 模板结构体。泛型枚举变为带判别字段的 C++ 带标签联合体。二者都必须在头文件中完整定义，因此导出的泛型函数以内联形式生成。

---

## 安全级别

Miva 为函数提供三种安全级别：

### 安全（默认）

```miva
// 所有函数默认都是安全的
main = () => {
  println("Hello");
}
```

安全函数**不能**：
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
- 使用 `deref` 与 `addr`
- 使用原始指针内建函数

### 可信

```miva
trusted safe_wrapper = (p: ptr<int>): int => {
  return deref p;
}
```

可信函数可执行不安全操作，但可被安全代码调用。它们作为不安全原语的安全抽象层。

### 安全限制流

```
安全函数 → 可调用：安全、可信（不可：不安全）
不安全函数 → 可调用：安全、不安全、可信
可信函数 → 可调用：安全、不安全、可信
```

---

## 移动语义与所有权

Miva 使用受 Rust 启发、带移动语义的所有权系统。

### 移动

```miva
// 移动转移所有权
main = () => {
  s := "hello";           // s 拥有该字符串
  consume(move s);        // s 被移动；s 失效
  printlns!(s);           // 错误：使用了已被移动的值 's'（E0001）
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

基础类型（`int`、`bool`、`float32`、`float64`、`char`）以及完全由复制类型组成的结构体自动复制 —— 无需显式 `clone`。

### 引用参数

`ref` 参数借用（而非移动）值。它们不可被移动：

```miva
bar = (ref x: int) => {
  move x;                // 错误：无法移动引用参数（E0002）
}
```

### 移动后重新赋值

对可变变量赋值会重置其状态，使其再次有效：

```miva
mut x := 42;
consume(move x);         // x 被移动
x = 99;                  // x 再次有效
```

### If/Choose 分支合并

在 `if` 表达式之后，若一个变量在**所有**分支中都被移动，则其在 `if` 之后视为已移动。若仅在一个分支中被移动，则仍然有效（因为两个分支都必须已移动它）。

---

## 异步

Miva 提供基于线程的异步模型：用 `async` 关键字声明的函数在被调用时会在其自身的操作系统线程上启动，并立即返回一个 `future[T]` 句柄；结果稍后通过 `.await()` 或 `await(...)` 获取，该操作会阻塞并合并（join）任务。

### 语法

`async` 函数必须将其返回类型标注为 `future[T]` —— 元素类型 `T` 即任务的返回类型：

```miva
async square = (x: int): future[int] => {
  return x * x;
}
```

调用 `async` 函数不会阻塞：它立即返回一个 `future[int]`。在该句柄上调用 `.await()`（或 `await(handle)`）会等待线程完成并产出内部的 `int`。

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
  f := square(5);                       // f 立即是一个 future[int]；任务在后台运行
  g := greet("miva");                   // 同上
  a := square(3).await();               // 阻塞直到 square(3) 完成
  b := square(4).await();
  printlns!(f.await(), g.await(), a, a, b);
  printlns!(combine(7).await());
  printlns!(add(square(2).await(), square(3).await()).await());
}
```

关键点：

- 调用 `async` 函数会**立即返回**一个 `future[T]`；任务在后台线程上并发运行。
- `.await()` 是 `await(...)` 的方法调用语法糖；两者等价。
- `.await()` 可链式调用（如 `combine` 内部），以组合多个异步任务。
- 对**非** future 值调用 `await(...)` 是恒等操作 —— 它原样返回值，因此 `await` 可安全包裹任何表达式。

### 类型

`future[T]` 是内置复合类型。`async` 函数声明的返回类型必须为 `future[T]` 形式，否则类型检查会拒绝它（"async function must return future[T]"）。类型参数 `T` 可以是任意 Miva 类型，包括 `string` 与结构体。

| 类型 | 描述 | C++ 映射 |
|------|-------------|-------------|
| `future[T]` | T 任务的句柄 | `mvp_future<T>` |

### 后端实现

- **C++（`cxx`）** —— `async` 函数编译为一个返回 `mvp_future<T>` 的包装器；函数体被捕获进一个 lambda，并通过 `std::async(std::launch::async)` 在 `std::future` 上经 `mvp_async_spawn` 运行。`.await()` 映射为 `mvp_async_await`，调用 `std::future::get()`。一个 `shared_ptr` 保持该 future 可复制，因此 `let f = task(); f.await()` 与 `task().await()` 都能工作。
- **LLVM（`llvm`）** —— 调用 `async` 函数会通过运行时桥 `miva_async_spawn`（一个基于 `std::thread` 的结构体）派生一个专用的操作系统线程，并返回一个任务句柄（i64）；`await(...)` 调用 `miva_async_await`，通过 `std::condition_variable` 等待并合并线程。
- **MVM（`mvm`）** —— 当目标是一个 `async` 函数时，`Call` 字节码会在新线程上派生一个全新的 `Mvm` 实例来运行该函数，压入一个 `Value::Future`（持有结果与线程句柄）。`await` 字节码（`Opcode::Await`）合并线程并取其结果。

### 安全性与并发语义

- `async` 函数默认是**安全**的；它们可调用其他安全 / 可信函数，并受移动/所有权规则约束。它们的参数按值捕获进后台线程（包括 `ref` 参数，会被复制以避免悬垂引用）。
- 异步任务与调用者在独立线程上并发运行；安全地共享不可变数据是程序员的职责 —— Miva 尚未提供语言级锁，因此互斥由标准库或 `inline unsafe` 的 C/C++ 代码提供。
- `await` 会阻塞当前线程直到 future 完成，因此对同一句柄多次 await 是安全且幂等的。

---

## 方法调用语法糖

方法调用语法 `receiver.method(args...)` 在编译时自动脱糖为 `method(receiver, args...)`。

```miva
twice = (x: int): int => x * 2
add = (a: int, b: int): int => a + b

main = () => {
  n := 10;

  // 无额外参数：n.twice() → twice(n)
  prints(n.twice())

  // 一个额外参数：n.add(5) → add(n, 5)
  prints(n.add(5))

  // 链式调用：n.twice().add(5) → add(twice(n), 5)
  prints(n.twice().add(5))

  // 嵌套方法调用的链式调用：
  // n.add(3).add(n.add(4)) → add(add(n, 3), add(n, 4))
  prints(n.add(3).add(n.add(4)))
}
```

方法调用语法支持泛型类型参数：

```miva
p.first[int, string]()          // → first[int, string](p)
```

---

## 宏

Miva 有两种宏：**内置宏**与**用户自定义宏**。

### 内置宏

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

与 `prints!` 相同，但追加一个结尾换行符。

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

在编译时读取文件，并将其内容作为字符串字面量内嵌。

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
greet!("World");   // 展开为问候块
```

宏语法：
- 参数以 `$` 为前缀：`$name`、`$x` 等。
- 参数具有显式类型：`($x: int, $y: string)`
- 宏体使用 `=>` 语法，与函数相同
- 在宏体内部，`$param` 引用变为 `EMacroVar` 节点，在展开时替换为参数表达式

宏在语义分析与类型检查**之前**展开。这意味着宏可用于任意表达式类型，且错误报告于展开后的代码上。

### 宏作用域

宏在编译**之前**于整个项目范围收集。项目中任意文件定义的宏对所有其他文件可用。`DMacro` 定义在展开后从 AST 中移除。

### 嵌套宏

宏可调用其他宏（包括嵌套内置宏）：

```miva
macro assert_eq = ($got: int, $expected: int) => {
  if ($got != $expected) {
    prints!("FAIL: expected ");
    printlns!($expected);
  } else {};
}
```

---

## 内置函数

Miva 提供约 80 个内置函数。这些函数在所有程序中无需导入即可使用。

### 输出函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `print` | `(s: string)` | 安全 | 打印字符串 |
| `prints` | `(s: string)` | 安全 | 打印字符串（已废弃，使用 `prints!` 宏） |
| `println` | `(s: string)` | 安全 | 打印字符串并换行 |
| `printlns` | `(s: string)` | 安全 | 打印字符串并换行（已废弃，使用 `printlns!` 宏） |
| `error` | `(s: string)` | 安全 | 打印到 stderr |
| `errors` | `(s: string)` | 安全 | 打印到 stderr（已废弃） |
| `errorln` | `(s: string)` | 安全 | 打印到 stderr 并换行 |
| `errorlns` | `(s: string)` | 安全 | 打印到 stderr 并换行（已废弃） |

### I/O 函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `read_int` | `(): int` | 安全 | 从 stdin 读取整数 |
| `read_line` | `(): string` | 安全 | 从 stdin 读取一行 |

### 控制函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `exit` | `(code: int)` | 安全 | 以指定退出码退出进程 |
| `abort` | `()` | 安全 | 中止进程 |
| `panic` | `(msg: string)` | 安全 | 以消息触发 panic（带消息中止） |

### 字符串函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `string_concat` | `(a: string, b: string): string` | 安全 | 拼接字符串（已废弃，使用 `std.str.concat`） |
| `string_parse` | `(s: string): int` | 安全 | 将字符串解析为整数（已废弃，使用 `std.str.parse_int`） |
| `string_length` | `(s: string): int` | 安全 | 获取字符串长度（已废弃，使用 `std.str.len`） |
| `string_make` | `(s: string, n: int): string` | 安全 | 创建字符串（已废弃，使用 `std.str.make`） |
| `string_from` | `(x: T): string` | 安全 | 将值转换为字符串 |
| `string_get` | `(s: string, i: int): char` | 安全 | 按索引获取字符串中的字符 |
| `to_string` | `(x: T): string` | 安全 | 转换为字符串（MVM 内置） |

### 盒子函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `box_new` | `(x: T): box<T>` | 安全 | 创建新盒子 |
| `box_deref` | `(b: box<T>): T` | 安全 | 解引用盒子 |
| `box_set` | `(b: box<T>, x: T)` | 安全 | 设置盒子内容 |

### Range 函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `range` | `(n: int): array<int>` | 安全 | 创建数组 `[0, 1, ..., n-1]` |

### 异步函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `await` | `(f: future<T>): T` | 安全 | 等待 future 结果 |

### 不安全指针函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `ptr_alloc` | `(size: int): ptrany` | 不安全 | 分配内存（已废弃，使用 `std.mem.alloc`） |
| `ptr_realloc` | `(p: ptrany, size: int): ptrany` | 不安全 | 重新分配内存（已废弃，使用 `std.mem.realloc`） |
| `ptr_free` | `(p: ptrany)` | 不安全 | 释放内存（已废弃，使用 `std.mem.mem_free`） |
| `ptr_set` | `(p: ptr<T>, v: T)` | 不安全 | 向指针写入值 |
| `ptr_offset` | `(p: ptrany, n: int): ptrany` | 不安全 | 将指针偏移 n 字节 |

### JSON 内置函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `json_parse` | `(s: string): ptrany` | 安全 | 将 JSON 字符串解析为不透明树 |
| `json_kind` | `(v: ptrany): int` | 安全 | JSON 节点种类（0=null，1=bool，2=number，3=string，4=array，5=object，-1=invalid） |
| `json_bool` | `(v: ptrany): bool` | 安全 | 提取 bool 值 |
| `json_number` | `(v: ptrany): float64` | 安全 | 提取 number 值 |
| `json_string` | `(v: ptrany): string` | 安全 | 提取 string 值 |
| `json_array_len` | `(v: ptrany): int` | 安全 | 数组长度 |
| `json_array_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引进数组元素 |
| `json_object_len` | `(v: ptrany): int` | 安全 | 对象键数量 |
| `json_object_key` | `(v: ptrany, i: int): string` | 安全 | 按索引获取对象键名 |
| `json_object_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引获取对象值 |
| `json_object_find` | `(v: ptrany, key: string): ptrany` | 安全 | 按键获取对象值 |
| `json_free` | `(v: ptrany)` | 安全 | 释放 JSON 树 |
| `json_stringify` | `(v: ptrany): string` | 安全 | 将 JSON 树序列化为字符串 |

### XML 内置函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `xml_parse` | `(s: string): ptrany` | 安全 | 将 XML 字符串解析为不透明树 |
| `xml_kind` | `(v: ptrany): int` | 安全 | XML 节点种类（0=null，1=element，2=text，3=comment，4=cdata，5=pi，6=document） |
| `xml_tag` | `(v: ptrany): string` | 安全 | 元素标签名 |
| `xml_attr_count` | `(v: ptrany): int` | 安全 | 属性数量 |
| `xml_attr_name` | `(v: ptrany, i: int): string` | 安全 | 按索引获取属性名 |
| `xml_attr_value` | `(v: ptrany, i: int): string` | 安全 | 按索引获取属性值 |
| `xml_attr_find` | `(v: ptrany, key: string): string` | 安全 | 按名称查找属性值 |
| `xml_child_count` | `(v: ptrany): int` | 安全 | 子节点数量 |
| `xml_child_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引获取子节点 |
| `xml_text` | `(v: ptrany): string` | 安全 | 文本内容 |
| `xml_comment` | `(v: ptrany): string` | 安全 | 注释内容 |
| `xml_cdata` | `(v: ptrany): string` | 安全 | CDATA 内容 |
| `xml_pi_target` | `(v: ptrany): string` | 安全 | 处理指令目标 |
| `xml_pi_data` | `(v: ptrany): string` | 安全 | 处理指令数据 |
| `xml_stringify` | `(v: ptrany): string` | 安全 | 将 XML 树序列化为字符串 |
| `xml_free` | `(v: ptrany)` | 安全 | 释放 XML 树 |

### TOML 内置函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `toml_parse` | `(s: string): ptrany` | 安全 | 将 TOML 字符串解析为不透明树 |
| `toml_kind` | `(v: ptrany): int` | 安全 | TOML 节点种类（同 JSON：0=null..5=object） |
| `toml_bool` | `(v: ptrany): bool` | 安全 | 提取 bool 值 |
| `toml_number` | `(v: ptrany): float64` | 安全 | 提取 number 值 |
| `toml_string` | `(v: ptrany): string` | 安全 | 提取 string 值 |
| `toml_array_len` | `(v: ptrany): int` | 安全 | 数组长度 |
| `toml_array_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引进数组元素 |
| `toml_object_len` | `(v: ptrany): int` | 安全 | 对象键数量 |
| `toml_object_key` | `(v: ptrany, i: int): string` | 安全 | 按索引获取对象键名 |
| `toml_object_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引获取对象值 |
| `toml_object_find` | `(v: ptrany, key: string): ptrany` | 安全 | 按键获取对象值 |
| `toml_free` | `(v: ptrany)` | 安全 | 释放 TOML 树 |
| `toml_stringify` | `(v: ptrany): string` | 安全 | 将 TOML 树序列化为字符串 |

### YAML 内置函数

| 函数 | 签名 | 安全性 | 描述 |
|----------|-----------|--------|-------------|
| `yaml_parse` | `(s: string): ptrany` | 安全 | 将 YAML 字符串解析为不透明树 |
| `yaml_kind` | `(v: ptrany): int` | 安全 | YAML 节点种类（同 JSON：0=null..5=object） |
| `yaml_bool` | `(v: ptrany): bool` | 安全 | 提取 bool 值 |
| `yaml_number` | `(v: ptrany): float64` | 安全 | 提取 number 值 |
| `yaml_string` | `(v: ptrany): string` | 安全 | 提取 string 值 |
| `yaml_array_len` | `(v: ptrany): int` | 安全 | 数组长度 |
| `yaml_array_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引进数组元素 |
| `yaml_object_len` | `(v: ptrany): int` | 安全 | 对象键数量 |
| `yaml_object_key` | `(v: ptrany, i: int): string` | 安全 | 按索引获取对象键名 |
| `yaml_object_get` | `(v: ptrany, i: int): ptrany` | 安全 | 按索引获取对象值 |
| `yaml_object_find` | `(v: ptrany, key: string): ptrany` | 安全 | 按键获取对象值 |
| `yaml_free` | `(v: ptrany)` | 安全 | 释放 YAML 树 |
| `yaml_stringify` | `(v: ptrany): string` | 安全 | 将 YAML 树序列化为字符串 |

### FFI（外部函数接口）

以 `ffi.` 为前缀的函数映射为 C++ 命名空间调用：

```miva
ffi.some_c_func(a, b);    // 编译为：some_c_func(a, b)
ffi.ns.func(args);        // 编译为：ns::func(args)
```

没有自动的 C 绑定生成；C 函数必须手动或通过 `c unsafe` 链接。

---

## 标准库

Miva 标准库（`std-0.1.2`）提供以下模块：

### `std.str` —— 字符串工具

```miva
import "std/str";

std.str.concat(ref a, ref b)       // 字符串拼接
std.str.parse_int(ref s)            // 字符串转整数解析
std.str.len(ref s)                  // 字符串长度
std.str.make(ref s, ref size)       // 字符串重复
std.str.from[T](x)                  // 值转字符串（泛型）
```

### `std.io` —— 带颜色 I/O

```miva
import "std/io";

std.io.cprint(ref x, ref color)     // 带颜色打印
std.io.cprintln(ref x, ref color)   // 带颜色打印行
std.io.eprint(ref x, ref color)     // 带颜色错误打印
std.io.eprintln(ref x, ref color)   // 带颜色错误打印行
```

颜色字符串来自 `std.term`（见下文）。

### `std.mem` —— 内存管理

```miva
import "std/mem";

std.mem.alloc(ref size): ptrany        // 分配内存
std.mem.realloc(ref p, size): ptrany    // 重新分配内存
std.mem.mem_free(ref p)                 // 释放内存
std.mem.offset(ref p, n): ptrany        // 将指针偏移 n 字节
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

### `std.vec` —— 可增长数组（向量）

```miva
import "std/vec";

// 类型
Vec[T] = struct { data: ptrany, len: int, cap: int }

// 构造
std.vec.new[T]()                        // 创建空 vec（不分配内存）
std.vec.with_capacity[T](cap)           // 创建带预分配容量的 vec

// 查询
std.vec.len[T](ref v)                   // 元素数量
std.vec.capacity[T](ref v)              // 当前容量（不重新分配）
std.vec.is_empty[T](ref v)              // vec 是否为空？
std.vec.elem_size[T](): int             // 元素字节大小

// 访问
std.vec.get[T](ref v, i)                // 按索引获取元素（越界 panic）
std.vec.get_unchecked[T](ref v, i)      // 按索引获取元素（无边界检查）

// 修改
std.vec.push[T](ref v, x)               // 追加元素
std.vec.pop[T](ref v): T                // 移除并返回最后一个元素
std.vec.set[T](ref v, i, x)             // 在索引处写入元素

// 内存管理
std.vec.free[T](ref v)                  // 释放底层缓冲区
std.vec.shrink_to_fit[T](ref v)         // 释放多余容量
std.vec.clear[T](ref v)                 // 清空但不释放缓冲区
std.vec.copy[T](ref v): Vec[T]          // 深拷贝
std.vec.truncate[T](ref v, new_len)     // 缩减 len 但不重新分配

// 内部
std.vec.grow[T](ref v, min_cap)         // 将缓冲区增长到至少 min_cap
```

大多数 `std.vec` 操作是 `unsafe` 的，因为它们解引用原始指针。

### `std.box` —— 装箱值

```miva
import "std/box";
```

`std.box` 模块目前是一个桩（空）。请改用内置函数 `box_new`、`box_deref`、`box_set`。

### `std.option` —— 可选值

```miva
import "std/option";

// 泛型可选值类型
Option[T] = enum { Some(T), None }

// 构造
std.option.some[T](v)                   // 将值包装进 Some
std.option.none[T](_dummy)              // 创建 None（需要虚拟 T 值）

// 查询
std.option.is_some[T](ref o): bool      // 是否为 Some？
std.option.is_none[T](ref o): bool      // 是否为 None？

// 解包
std.option.expect[T](ref o, msg): T     // 解包或带消息 panic
std.option.unwrap[T](ref o): T          // 解包或 panic
std.option.unwrap_or[T](ref o, default): T  // 解包或返回默认值

// 比较
std.option.contains[T](ref o, x): bool  // 是否为包含 x 的 Some？

// 转换
std.option.flatten[T](ref o): Option[T] // 将 Option[Option[T]] 扁平化为 Option[T]
```

### `std.result` —— 结果值

```miva
import "std/result";

// 泛型结果类型
Result[T, E] = enum { Ok(T), Err(E) }

// 构造
std.result.ok[T, E](v)                  // 将值包装进 Ok
std.result.err[T, E](e)                 // 将错误包装进 Err

// 查询
std.result.is_ok[T, E](ref r): bool     // 是否为 Ok？
std.result.is_err[T, E](ref r): bool    // 是否为 Err？

// 解包
std.result.expect[T, E](ref r, msg): T  // 解包或带消息 panic
std.result.unwrap[T, E](ref r): T       // 解包 Ok 或 panic
std.result.unwrap_or[T, E](ref r, fallback): T  // 解包或返回 fallback

// 转换
std.result.map_err[T, E, F](ref r, e): Result[T, F]  // 将错误映射为新类型

// 组合
std.result.and[T, E, U](ref r, other): Result[U, E]   // 在 Ok 上链式
std.result.or[T, E, F](ref r, other): Result[T, F]    // 在 Err 上回退
```

### `std.json` —— JSON 解析

```miva
import "std/json";

// 种类标签：0=null 1=bool 2=number 3=string 4=array 5=object

std.json.parse(ref s): ptrany               // 解析 JSON 字符串 → 不透明句柄
std.json.kind(ref v): int                   // 节点种类标签
std.json.is_null(ref v): bool               // 种类谓词
std.json.is_bool(ref v): bool               // 种类谓词
std.json.is_number(ref v): bool             // 种类谓词
std.json.is_string(ref v): bool             // 种类谓词
std.json.is_array(ref v): bool              // 种类谓词
std.json.is_object(ref v): bool             // 种类谓词

std.json.as_bool(ref v): bool               // 提取 bool（不匹配时 panic）
std.json.as_number(ref v): float64          // 提取 number（不匹配时 panic）
std.json.as_string(ref v): string           // 提取 string（不匹配时 panic）

std.json.len(ref v): int                    // 数组长度或对象键数量
std.json.array_get(ref v, i): ptrany         // 按索引获取数组元素
std.json.object_get(ref v, i): ptrany        // 按索引获取对象值
std.json.object_key(ref v, i): string        // 按索引获取对象键名
std.json.object_find(ref v, key): ptrany     // 按键获取对象值

std.json.stringify(ref v): string           // 序列化为紧凑 JSON
std.json.free(ref v)                        // 释放树
```

### `std.xml` —— XML 解析

```miva
import "std/xml";

// 种类标签：0=null 1=element 2=text 3=comment 4=cdata 5=pi 6=document

std.xml.parse(ref s): ptrany                // 解析 XML 字符串 → 不透明句柄
std.xml.kind(ref v): int                    // 节点种类标签
std.xml.is_element(ref v): bool             // 种类谓词
std.xml.is_text(ref v): bool                // 种类谓词
std.xml.is_comment(ref v): bool             // 种类谓词
std.xml.is_cdata(ref v): bool               // 种类谓词
std.xml.is_pi(ref v): bool                  // 种类谓词
std.xml.is_document(ref v): bool            // 种类谓词

std.xml.tag(ref v): string                  // 元素标签名
std.xml.attr_count(ref v): int              // 属性数量
std.xml.attr_name(ref v, i): string          // 按索引获取属性名
std.xml.attr_value(ref v, i): string         // 按索引获取属性值
std.xml.attr_find(ref v, key): string        // 按名称查找属性值
std.xml.child_count(ref v): int             // 子节点数量
std.xml.child_get(ref v, i): ptrany          // 按索引获取子节点

std.xml.text(ref v): string                 // 文本内容
std.xml.comment(ref v): string             // 注释内容
std.xml.cdata(ref v): string               // CDATA 内容
std.xml.pi_target(ref v): string            // 处理指令目标
std.xml.pi_data(ref v): string              // 处理指令数据

std.xml.stringify(ref v): string            // 序列化为 XML 文本
std.xml.free(ref v)                         // 释放树
```

### `std.toml` —— TOML 解析

与 `std.json` 相同的树形 API 结构：

```miva
import "std/toml";

std.toml.parse(ref s): ptrany
std.toml.kind(ref v): int
std.toml.is_null/bool/number/string/array/object(ref v): bool
std.toml.as_bool/number/string(ref v)
std.toml.len(ref v): int
std.toml.array_get(ref v, i): ptrany
std.toml.object_get/object_key/object_find(ref v, ...)
std.toml.stringify(ref v): string
std.toml.free(ref v)
```

### `std.yaml` —— YAML 解析

与 `std.json` 相同的树形 API 结构：

```miva
import "std/yaml";

std.yaml.parse(ref s): ptrany
std.yaml.kind(ref v): int
std.yaml.is_null/bool/number/string/array/object(ref v): bool
std.yaml.as_bool/number/string(ref v)
std.yaml.len(ref v): int
std.yaml.array_get(ref v, i): ptrany
std.yaml.object_get/object_key/object_find(ref v, ...)
std.yaml.stringify(ref v): string
std.yaml.free(ref v)
```

### `std.future` —— Future 工具

```miva
import "std/future";
```

`std.future` 模块目前是一个桩（空）。请直接使用内置的 `await` 函数与 `future[T]` 类型。

---

## C FFI（外部函数接口）

Miva 允许通过 `inline unsafe` 函数语法内嵌原始 C++ 代码：

```miva
// 使用 "inline" 关键字（推荐）
inline unsafe printf_wrapper = (fmt: string): int => {
  return printf("%s", fmt);
}

// 使用 "c" 关键字（已废弃，会产生 W0004 警告）
c unsafe puts = (s: string): int => {
  return puts(s);
}
```

`{ }` 之间的 C++ 代码被直接插入到生成的 C++ 翻译单元中。在 `llvm` 与 `mvm` 后端上，`inline`/`c` 原始块不可用，此类函数必须由外部提供或省略。

### 无花括号的原始 C 函数（字符串体）

```miva
inline unsafe custom_fn = (x: int): int => "return x * 2;"
```

这避免了花括号限定的原始块，使用字符串字面量作为函数体。

---

## 运算符重载

支持通过 `impl` 块进行运算符重载：

```miva
impl Point {
  op_add my_add,    // my_add(a, b) → a + b
  op_sub my_sub,    // my_sub(a, b) → a - b
  op_mul my_mul,    // my_mul(a, b) → a * b
  op_div my_div,    // my_div(a, b) → a / b
  op_eq my_eq,      // my_eq(a, b) → a == b
  op_neq my_neq,    // my_neq(a, b) → a != b
}
```

这会为该结构体类型生成 C++ 的 `operator+`、`operator-`、`operator*`、`operator/`、`operator==` 与 `operator!=` 函数，委派给具名函数。

---

## 编译器流程与命令

### 流程

```
源文件 (.miva)
  ↓ 词法分析与语法分析 (miva-frontend-rs)
JSON AST
  ↓ 全项目收集宏
  ↓ 宏展开（内置 + 用户自定义）
  ↓ 收集函数签名（跨模块类型解析）
  ↓ 符号表构建
  ↓ 语义分析
    • 变量解析
    • 移动语义
    • 安全性强制检查
    • 模块/导入校验
  ↓ 类型检查
    • 类型推断
    • 泛型类型替换
    • Lambda 捕获标注
    • 类型一致性验证
  ↓ 警告生成与过滤
  ↓ 代码生成（C++ / LLVM IR / MVM 字节码）
C++ 源码 (.cpp, .h)        LLVM IR (.ll)          MVM 字节码 (.mvm)
  ↓ g++ (C++20)               ↓ llc + g++ 链接器         ↓ mvm 解释器
  ↓ 目标文件 (.o)                                    （无需原生链接器）
  ↓ 链接
原生二进制 (.exe / .so)
```

### 命令

| 命令 | 描述 |
|---------|-------------|
| `miva init <name> -t <bin\|lib>` | 初始化新项目 |
| `miva reinit` | 从模板重新生成 `miva.toml` 并移除 `miva.lock` |
| `miva build [-b <cxx\|llvm\|mvm>]` | 构建项目 |
| `miva run [-b <cxx\|llvm\|mvm>]` | 构建并运行（`-b mvm` / `--mvm` 在解释器上运行） |
| `miva clean` | 清理构建产物 |
| `miva sin-build <file>` | 编译单个文件 |
| `miva sin-run <file>` | 编译并运行单个文件 |
| `miva test <file>` | 运行测试文件 |
| `miva get <url>` | 安装依赖 |
| `miva dep` | 从 `main.miva` 起展示依赖图 |
| `miva <script>` | 运行 `[scripts]` 中定义的自定义脚本 |

选项：
- `--release` —— 发布模式（优化，`-O2`）
- `--verbose` / `-v` —— 详细输出
- `-b <backend>` / `--backend <backend>` —— 后端：`cxx`（默认）、`llvm` 或 `mvm`
- `--mvm` —— 等价于 `-b mvm`；构建字节码并在 MVM 解释器上运行

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
│   ├── main.miva.cpp       # 生成的 C++ 源码（cxx 后端）
│   ├── main.miva.h         # 生成的 C++ 头文件（导出）
│   ├── main.miva.ll        # 生成的 LLVM IR（llvm 后端）
│   ├── main.miva.mvm       # 生成的字节码（mvm 后端）
│   ├── main.miva.o         # 编译后的目标文件
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

| 代码 | 描述 |
|------|-------------|
| E0001 | 使用了已被移动的值 |
| E0002 | 无法移动引用参数 / 无法对不可变变量赋值 |
| E0004 | 重复的函数或结构体定义 |
| E0005 | 模块声明必须位于顶部 / 只能有一个模块 / 重复的模块 |
| E0007 | 未找到变量 |
| E0009 | 无法从安全代码调用不安全函数 / 未知函数 |
| E0010 | 无法在安全函数中解引用指针 |
| E0011 | Choose 表达式必须有 otherwise 分支 |
| E0013 | 无效的魔法注释 |

### 类型错误

| 代码 | 描述 |
|------|-------------|
| E0014 | 类型不匹配 / 需要非 void 处出现 void 值 / 二元运算符类型不匹配 / if 条件非 bool / 分支类型不匹配 / 对非指针解引用 / 对非结构体进行字段访问 |
| E0016 | 函数参数数量或类型不匹配 / 枚举变体负载长度不匹配 |
| E0017 | 返回类型不匹配 / 异步函数体与其声明的 future 元素类型不匹配 |
| E0018 | 结构体字面量错误（未知结构体 / 字段类型错误 / 缺少字段） |
| E0019 | 结构体中未知字段 / 枚举中未知变体 / 负载索引越界 |
| E0020 | Lambda 体类型与声明的返回类型不匹配 / 异步函数必须返回 future[T] |
| E0021 | 无效的强制转换 |
| E0022 | let 声明或赋值中的类型不匹配 |
| E0024 | 所有数组元素必须具有相同类型 |
| E0026 | for-each 循环范围必须是数组 |

---

## 警告码

| 代码 | 描述 |
|------|-------------|
| W0001 | 命名约定违规（非 snake_case 的函数/变量，非小写的模块） |
| W0002 | 使用了已废弃的函数（使用 std 库替代） |
| W0003 | 无效的引导注释注解 |
| W0004 | 使用了已废弃的关键字（`c` 关键字 → 改用 `inline`） |

警告可通过魔法指令控制：

```miva
/! warning_off W0001    // 抑制命名警告
/! warning_err W0002    // 将废弃警告视为错误
```

---

## 已废弃函数

| 函数 | 替代 |
|----------|-------------|
| `prints` | 宏 `prints!` |
| `printlns` | 宏 `printlns!` |
| `string_concat` | `std.str.concat` |
| `string_parse` | `std.str.parse_int` |
| `string_length` | `std.str.len` |
| `string_make` | `std.str.make` |
| `ptr_alloc` | `std.mem.alloc` |
| `ptr_realloc` | `std.mem.realloc` |
| `ptr_free` | `std.mem.mem_free` |

## 术语表

- **AST** —— 抽象语法树，源代码结构的内部表示
- **FFI** —— 外部函数接口，调用 C/C++ 函数的机制
- **Move** —— 转移值的所有权，使源失效
- **Clone** —— 值的显式复制，源仍然有效
- **Ref** —— 对值的借用引用（按 const 引用传递）
- **Own** —— 拥有的参数（按值传递，所有权转移）
- **Box** —— 具有自动生命周期管理的堆分配值
- **Magical** —— 控制警告、发布模式等的编译器指令
- **Intro** —— 标注下一个定义的安全性/用法的注解注释
- **Sir (sin-)** —— 单文件编译（无需项目配置）
- **Enum** —— 带具名变体的代数数据类型 / 带标签联合体
- **Closure** —— 带有捕获环境的匿名函数
- **Guard** —— 模式匹配分支上的额外条件
