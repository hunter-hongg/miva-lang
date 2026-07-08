# Miva 编程语言

Miva 是一种编译型系统编程语言。它经由 C++ 转译，最终通过 g++ 编译为本机可执行文件。Miva 具备强静态类型系统、泛型编程、模式匹配、移动语义、安全级别体系、宏以及零开销的 C 外部函数接口（FFI）。

## 目录

- [概述](#概述)
- [快速开始](#快速开始)
- [项目配置](#项目配置)
- [注释](#注释)
- [模块系统](#模块系统)
- [定义](#定义)
- [类型系统](#类型系统)
- [变量](#变量)
- [表达式](#表达式)
- [语句](#语句)
- [控制流](#控制流)
- [函数](#函数)
- [结构体](#结构体)
- [泛型](#泛型)
- [安全级别](#安全级别)
- [移动语义与所有权](#移动语义与所有权)
- [方法调用语法糖](#方法调用语法糖)
- [宏](#宏)
- [内置函数](#内置函数)
- [标准库](#标准库)
- [C FFI（外部函数接口）](#c-ffi外部函数接口)
- [运算符重载](#运算符重载)
- [编译器管线与命令](#编译器管线与命令)
- [错误码](#错误码)
- [警告码](#警告码)

---

## 概述

Miva 是一个完整的编程语言编译器栈：

1. **前端**（`miva-frontend-rs`）—— 对 `.miva` 源文件进行词法分析和语法分析，生成 JSON 格式的抽象语法树（AST）。
2. **编译器**（`miva`）—— 加载 JSON AST，依次执行宏展开、语义分析、类型检查，并生成 C++ 代码。
3. **后端**（`g++`）—— 将生成的 C++ 代码编译为本地机器码（`.o` 文件），并链接为可执行文件或共享库。

编译管线：

```
.miva 源码 → 词法/语法分析 → JSON AST
  → 宏展开 → 符号表 → 语义分析
  → 类型检查 → C++ 代码生成 → g++ → 本地二进制
```

---

## 快速开始

### 构建

```bash
# 克隆仓库
git clone <repo-url>
cd miva-lang

# 构建前端和编译器
./build.sh --release
```

构建完成后，将 `miva-frontend-rs/target/release/miva-frontend` 和 `miva/target/release/miva` 加入 `PATH`。

### 创建新项目

```bash
# 初始化名为 "myapp" 的项目
miva init myapp
```

这会创建以下结构：

```
myapp/
├── miva.toml      # 项目配置文件
└── src/
    └── main.miva  # 入口文件
```

### 构建和运行

```bash
# 构建项目
miva build --release

# 构建并运行
miva run --release

# 编译单个文件（快速测试）
miva sin-build path/to/file.miva
miva sin-run path/to/file.miva

# 清理构建产物
miva clean

# 运行测试
miva test <test_file.miva>
```

### Hello, World!

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

每个 Miva 项目必须在根目录下包含 `miva.toml`：

```toml
[project]
name = "myapp"
type = "exe"         # "exe" 为可执行文件，"lib" 为共享库
version = "0.1.0"

[dependencies]
std = "0.1.0"        # 标准库依赖
```

### 项目类型

- **`exe`** —— 编译为本机可执行文件，需要 `main()` 入口函数。
- **`lib`** —— 编译为共享库（`.so`），使用 `-fPIC` 和 `-shared` 链接选项。入口文件为 `src/lib.miva`。

### 依赖管理

依赖从标准库路径获取。标准库以 `std-0.1.0` 形式打包：

```toml
[dependencies]
std = "0.1.0"
```

可从 GitHub 安装依赖：

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
 * /* 嵌套也OK */
 */

/! 魔法指令（控制编译器行为）
```

### 魔法指令（Magical Directives）

```miva
/! warning_off W0001    // 抑制警告 W0001
/! warning_err W0002    // 将警告 W0002 视为错误
/! release always       // 标记为仅发布模式
/! mangle name          // 自定义名称混淆
```

### 引言注释（Intro Comments / 注解）

```miva
@ unsafe: 执行原始内存操作
@ usage: 用作内部辅助函数
@ param: x 是输入值
@ impl: 为结构体实现某个特征
@ trusted: 不安全代码的安全封装
```

引言注释用于注解下一个定义，编译器会验证其正确性（例如 `unsafe` 注解只能在 `unsafe` 函数前使用，`usage` 可以在任何定义前使用等）。

---

## 模块系统

每个 Miva 文件必须声明且只能声明一个模块：

```miva
module main;          // 简单模块
module std.io;        // 命名空间模块（C++ 中生成 mvp_std::io）
module my.app.utils;  // 深层命名空间
```

模块声明**必须**出现在文件顶部，位于任何其他定义之前。

### 导入（Import）

```miva
// 基本导入
import "std/str";

// 带命名空间别名的导入
import "std/io" as io;

// 导入并引入当前命名空间
import "std/io" as .;

// 导入 C 头文件（生成 #include <stdio.h>）
import "c:stdio.h";
```

导入路径解析规则：
- `proj_name/path` —— 项目内部：解析为 `src/path.miva`
- `std/path` —— 标准库：解析到标准库包含目录
- `library/path` —— 外部依赖
- `c:header.h` —— C 头文件（生成 `#include <header.h>`）

### 导出（Export）

```miva
export my_function;
export my_struct;
```

导出的符号对其他模块可见。泛型函数作为 C++ 模板生成在头文件中；非泛型函数在头文件中声明，在源文件中定义。

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

// 单表达式函数（不需要花括号）
double = (x: int): int => x * 2

// 无返回值函数（不指定返回类型）
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

测试被单独编译为测试可执行文件。必须返回 `int` 类型。

---

## 类型系统

### 基本类型

| 类型 | 描述 | C++ 映射 |
|------|------|----------|
| `int` | 有符号整数 | `mvp_builtin_int` |
| `bool` | 布尔值 | `mvp_builtin_boolean` |
| `float32` | 32 位浮点数 | `mvp_builtin_float` |
| `float64` | 64 位浮点数 | `mvp_builtin_float` |
| `char` | 字符（字节） | `mvp_builtin_byte` |
| `string` | 字符串 | `mvp_builtin_string` |

### 复合类型

| 类型 | 描述 | C++ 映射 |
|------|------|----------|
| `array<T>` | T 的数组/向量 | `std::vector<T>` |
| `ptr<T>` | 指向 T 的指针 | `T*` |
| `box<T>` | 堆分配的 T | `mvp_builtin_box<T>` |
| `ptrany` | 无类型指针 | `mvp_builtin_ptrany` |
| `null` | 空/无值 | `void` |

### 结构体类型

通过名称引用结构体类型，可附带泛型类型参数：

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
x = 20;     // OK：x 是可变的

// 错误：不能给不可变变量赋值
y := 10;
y = 20;     // 编译错误
```

### 移动（Move）和克隆（Clone）

```miva
// 移动所有权
move x;                // x 被移动，之后不能再使用

// 克隆（复制）值
clone x;               // x 仍然有效
```

基本类型（int、bool、float32、float64、char）是复制类型（copy type），不需要显式 `clone`。仅包含基本类型字段的结构体也是复制类型。字符串、数组、指针和盒子（box）是移动类型（move type）。

---

## 表达式

### 字面量

```miva
42            // 整数
3.14          // 浮点数
true          // 布尔真
false         // 布尔假
'a'           // 字符
"hello"       // 字符串
"""           // 多行字符串字面量
第一行
第二行
"""           // （两个 """ 之间的内容）
```

### 二元运算符

| 运算符 | 描述 | 操作数类型 |
|--------|------|-----------|
| `+` | 加法/字符串拼接 | int, float32, float64, string |
| `-` | 减法 | int, float32, float64 |
| `*` | 乘法 | int, float32, float64 |
| `==` | 相等比较 | 所有可比类型 |
| `!=` | 不等比较 | 所有可比类型 |

运算符优先级（从低到高）：
1. `==`、`!=`
2. `+`、`-`
3. `*`

### 一元运算符

```miva
addr x     // 取地址：返回 ptr<T>
deref p    // 解引用：需要 ptr<T>
```

### 类型转换表达式

```miva
x as int           // 转换为 int
y as float64       // 转换为 float64
c as char          // 转换为 char
```

有效转换：
- `int ↔ float32`、`int ↔ float64`、`float32 ↔ float64`
- `int ↔ char`
- `bool → int`
- 同类型转换（恒等）

### If 表达式

```miva
// 不带 else 的 if（返回 void/null）
if (condition) {
  do_something();
};

// if-else（两个分支必须类型相同）
result := if (condition) {
  10
} else {
  20
};
```

`if` 是表达式，可以返回值。当两个分支都有返回值时，类型必须相同。如果没有 `else` 分支，表达式产生 `null`。

### Choose（模式匹配）

```miva
choose (x) {
  when (1) { println("一"); }
  when (2) { println("二"); }
  otherwise { println("其他"); }
};
```

- 被匹配的变量和 `when` 值必须类型相同。
- 所有分支必须类型相同。
- `otherwise` 分支**必须存在**——省略将导致编译错误 E0011。

### 块（Block）

```miva
// 块表达式返回最后一个表达式的值
result := {
  let x int = 10;
  let y int = 20;
  x + y         // ← 块的结果
};

// 无返回值的块
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

任何表达式后跟 `;` 即为语句：

```miva
println("测试");
x + 1;
```

### Return 语句

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

注意：当用作语句时，闭合的 `}` 后需要跟 `;`。

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

for-in 循环遍历数组。循环变量的类型是数组的元素类型。

---

## 函数

### 函数语法

```miva
// name = (params): return_type => 表达式
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
// Own 参数（所有权转移）
foo = (x: int, y: string) => { ... }

// Ref 参数（借用，常量引用）
bar = (ref x: int, ref s: string) => { ... }
```

- **`ref`** 参数在 C++ 中以 `const&` 传递——不发生所有权转移。
- **`own`** 参数（默认）接收所有权，参数可以被移动。

### 函数调用

```miva
// 普通调用
add(3, 4);

// 带显式类型参数的调用（泛型函数）
identity[int](42);
mk_pair[int, string](1, "one");

// 方法调用语法（解糖为普通函数调用）
x.twice()                   // → twice(x)
n.add(5)                    // → add(n, 5)
n.add(3).add(4)             // → add(add(n, 3), 4)

// 带类型参数的方法调用
p.first[int, string]()      // → first[int, string](p)
```

方法调用语法自动将接收者作为第一个参数插入。

### 泛型函数

```miva
// 单个类型参数
identity[T] = (x: T): T => x

// 多个类型参数
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// 调用时指定类型参数
let p Pair[int, string] = mk_pair[int, string](1, "one");
```

类型参数通常可以从参数中推断：

```miva
let x = identity[int](42);   // 显式指定
let y = identity(42);        // 类型推断（如果编译器能推导出 T）
```

### 嵌套函数（函数都是顶层定义）

所有函数定义都是顶层（top-level）的。块中不支持嵌套函数表达式。

### 递归

函数可以递归调用自身。不保证完全的尾调用优化。

---

## 结构体

### 结构体定义

```miva
Point = struct {
  x: int,
  y: int,
}
```

### 字段访问

```miva
p.x       // 访问字段
p.y
```

### 结构体字面量

```miva
let p Point = struct Point { x = 10, y = 20 };

// 泛型结构体字面量
let b Box[int] = struct Box[int] { value = 42 };
```

结构体字面量使用 `struct TypeName { field = value, ... }` 语法，字段之间用逗号分隔。

---

## 泛型

Miva 支持结构体和函数的泛型编程。

### 泛型结构体

```miva
Box[T] = struct {
  value: T,
}

Pair[T, U] = struct {
  first: T,
  second: U,
}

// 无字段的泛型结构体
Empty[T] = struct {}
```

### 泛型函数

```miva
// 使用泛型类型的泛型函数
mk_box[T] = (x: T): Box[T] => struct Box[T] { value = x }

// 多类型参数的泛型函数
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// 嵌套泛型
mk_nested[T, U] = (a: T, b: U): Box[Pair[T, U]] => struct Box[Pair[T, U]] { value = mk_pair[T, U](a, b) }
```

类型参数使用方括号语法：`func[T, U](args)`。

泛型函数编译为 C++ 模板。泛型结构体编译为 C++ 模板结构体。两者都必须在头文件中完全定义，因此导出的泛型函数以内联方式生成。

---

## 安全级别

Miva 为函数提供三个安全级别：

### Safe（默认）

```miva
// 所有函数默认为 safe
main = () => {
  println("Hello");
}
```

Safe 函数**不能**：
- 调用 `unsafe` 函数
- 解引用指针（`deref` 表达式）
- 使用原始指针内置函数（`ptr_alloc`、`ptr_realloc`、`ptr_free`、`ptr_set`）

### Unsafe

```miva
unsafe dangerous_op = (p: ptr<int>) => {
  deref p;
}
```

Unsafe 函数可以：
- 调用其他 unsafe 函数
- 使用 `deref` 和 `addr`
- 使用原始指针内置函数

### Trusted

```miva
trusted safe_wrapper = (p: ptr<int>): int => {
  return deref p;
}
```

Trusted 函数可以执行不安全操作，但可以在 safe 代码中调用。它们作为不安全原语的安全抽象层。

### 安全限制流程

```
safe 函数 → 可调用：safe, trusted（不能调用 unsafe）
unsafe 函数 → 可调用：safe, unsafe, trusted
trusted 函数 → 可调用：safe, unsafe, trusted
```

---

## 移动语义与所有权

Miva 使用受 Rust 启发的所有权系统，具有移动语义。

### 移动（Move）

```miva
// Move 转移所有权
main = () => {
  s := "hello";           // s 拥有字符串的所有权
  consume(move s);        // s 被移动；s 变为无效
  printlns!(s);           // 错误：使用了已移动的值 's'（E0001）
}
```

### 克隆（Clone）

```miva
main = () => {
  s := "hello";
  consume(clone s);       // s 被克隆；s 仍然有效
  printlns!(s);           // OK
}
```

### 复制类型（Copy Types）

基本类型（`int`、`bool`、`float32`、`float64`、`char`）以及完全由复制类型组成的结构体会自动复制——无需显式 `clone`。

### Ref 参数

`ref` 参数借用（borrow）值而非移动。它们不能被移动：

```miva
bar = (ref x: int) => {
  move x;                // 错误：不能移动 ref 参数（E0002）
}
```

### 移动后赋值

对可变变量赋值会重置其状态，使其重新有效：

```miva
mut x := 42;
consume(move x);         // x 被移动
x = 99;                  // x 重新有效
```

### If/Choose 分支合并

在 `if` 表达式之后，如果某个变量在**所有**分支中都被移动了，则被视为已移动。如果只在一个分支中被移动，则仍然有效（因为另一个分支也必须有移动操作）。

---

## 方法调用语法糖

方法调用语法 `receiver.method(args...)` 在编译时自动解糖为 `method(receiver, args...)`。

```miva
twice = (x: int): int => x * 2
add = (a: int, b: int): int => a + b

main = () => {
  n := 10;

  // 零个额外参数：n.twice() → twice(n)
  prints(n.twice())

  // 一个额外参数：n.add(5) → add(n, 5)
  prints(n.add(5))

  // 链式调用：n.twice().add(5) → add(twice(n), 5)
  prints(n.twice().add(5))

  // 链式调用带嵌套方法调用：
  // n.add(3).add(n.add(4)) → add(add(n, 3), add(n, 4))
  prints(n.add(3).add(n.add(4)))
}
```

方法调用支持泛型类型参数：

```miva
p.first[int, string]()          // → first[int, string](p)
```

---

## 宏

Miva 有两种宏：**内置宏**和**用户自定义宏**。

### 内置宏

#### `prints!(...)`

打印多个值，用空格分隔。自动将值转换为字符串。

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

与 `prints!` 相同，但添加尾随换行。

```miva
printlns!(1, 2, 3);    // 输出："1 2 3\n"
```

#### `assert!(expr)`

如果表达式求值为 `false`，则调用 `panic("Assertion failed")`。

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

在编译时读取文件，并将其内容嵌入为字符串字面量。

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
- 参数有显式类型：`($x: int, $y: string)`
- 宏体使用 `=>` 语法，与函数类似
- 在宏体内，`$param` 引用会变成 `EMacroVar` 节点，在展开时被参数表达式替换

宏在**语义分析和类型检查之前**展开。这意味着宏可以处理任何表达式类型，错误报告针对展开后的代码。

### 宏的作用域

宏在**编译前**项目范围内收集。项目中任何文件中定义的宏对所有其他文件都可用。`DMacro` 定义在展开后从 AST 中移除。

### 嵌套宏

宏可以调用其他宏（包括嵌套的内置宏）：

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

### 输出函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `print` | `(s: string)` | 打印字符串 |
| `prints` | `(s: string)` | 打印字符串（已弃用，使用 `prints!`） |
| `println` | `(s: string)` | 打印字符串并换行 |
| `printlns` | `(s: string)` | 打印字符串并换行（已弃用，使用 `printlns!`） |
| `error` | `(s: string)` | 打印到 stderr |
| `errors` | `(s: string)` | 打印到 stderr（已弃用） |
| `errorln` | `(s: string)` | 打印到 stderr 并换行 |
| `errorlns` | `(s: string)` | 打印到 stderr 并换行（已弃用） |

### 控制函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `exit` | `(code: int)` | 以指定码退出进程 |
| `abort` | `()` | 中止进程 |
| `panic` | `(msg: string)` | 带消息崩溃（中止并打印消息） |

### 字符串函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `string_concat` | `(a: string, b: string): string` | 拼接字符串（已弃用） |
| `string_parse` | `(s: string): int` | 解析字符串为整数（已弃用） |
| `string_length` | `(s: string): int` | 获取字符串长度（已弃用） |
| `string_make` | `(s: string, n: int): string` | 生成字符串（已弃用） |
| `string_from` | `(x: T): string` | 将值转换为字符串 |

### Box 函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `box_new` | `(x: T): box<T>` | 创建新 box |
| `box_deref` | `(b: box<T>): T` | 解引用 box |

### Range 函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `range` | `(n: int): array<int>` | 创建数组 `[0, 1, ..., n-1]` |

### 不安全指针函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `ptr_alloc` | `(size: int): ptrany` | 分配内存 |
| `ptr_realloc` | `(p: ptrany, size: int): ptrany` | 重新分配内存 |
| `ptr_free` | `(p: ptrany)` | 释放内存 |
| `ptr_set` | `(p: ptrany, v: int)` | 写入指针 |

所有指针函数都是 **unsafe** 的，只能在 `unsafe` 或 `trusted` 函数中使用。

### FFI（外部函数接口）

以 `ffi.` 为前缀的函数映射到 C++ 命名空间调用：

```miva
ffi.some_c_func(a, b);    // 编译为：some_c_func(a, b)
ffi.ns.func(args);        // 编译为：ns::func(args)
```

没有自动的 C 绑定生成；C 函数必须手动链接或通过 `c unsafe` 嵌入。

---

## 标准库

Miva 标准库（`std-0.1.0`）提供：

### `std.str` —— 字符串工具

```miva
import "std/str";

std.str.concat(ref a, ref b)      // 字符串拼接
std.str.parse_int(ref s)           // 字符串转整数
std.str.len(ref s)                 // 字符串长度
std.str.make(ref s, ref size)     // 字符串重复
std.str.from[T](x)                // 值转字符串（泛型）
```

`from[T]` 的类型参数已导出，可在模块间使用。

### `std.io` —— 彩色 I/O

```miva
import "std/io";

std.io.cprint(ref x, ref color)     // 带颜色打印
std.io.cprintln(ref x, ref color)   // 带颜色打印并换行
std.io.eprint(ref x, ref color)     // 带颜色错误打印
std.io.eprintln(ref x, ref color)   // 带颜色错误打印并换行
```

### `std.mem` —— 内存管理

```miva
import "std/mem";

std.mem.alloc(ref size)             // 分配内存（返回 ptrany）
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

### `std.vec` / `std.box`

这些模块是未来功能的占位符，目前为空。

---

## C FFI（外部函数接口）

Miva 允许通过 `c unsafe` 函数语法嵌入原始 C++ 代码：

```miva
// 使用 "c" 关键字（已弃用，会生成 W0004 警告）
c unsafe puts = (s: string): int => {
  return puts(s);
}

// 使用 "inline" 关键字（推荐）
inline unsafe printf_wrapper = (fmt: string): int => {
  return printf("%s", fmt);
}
```

花括号 `{ }` 之间的 C++ 代码直接被插入到生成的 C++ 翻译单元中。

### 无花括号的 C 函数（字符串体）

```miva
c unsafe custom_fn = (x: int): int => "return x * 2;"
```

这种形式避免使用花括号分隔的原始块，而是使用字符串字面量作为函数体。

---

## 运算符重载

通过 `impl` 块支持运算符重载：

```miva
impl Point {
  op_add my_add,    // my_add(a, b) → a + b
  op_sub my_sub,    // my_sub(a, b) → a - b
  op_mul my_mul,    // my_mul(a, b) → a * b
  op_eq my_eq,      // my_eq(a, b) → a == b
  op_neq my_neq,    // my_neq(a, b) → a != b
}
```

这会为该结构体类型生成 C++ 的 `operator+`、`operator-`、`operator*`、`operator==` 和 `operator!=` 函数，委托给指定的命名函数。

---

## 编译器管线与命令

### 管线

```
源码 (.miva)
  ↓ 词法分析 & 语法分析（miva-frontend-rs）
JSON AST
  ↓ 项目范围内收集宏定义
  ↓ 宏展开（内置宏 + 用户自定义宏）
  ↓ 符号表构建
  ↓ 语义分析
    • 变量解析
    • 移动语义检查
    • 安全检查（safe/unsafe/trusted 限制）
    • 模块/导入验证
  ↓ 类型检查
    • 类型推断
    • 泛型类型替换
    • 类型一致性验证
  ↓ 警告生成与过滤
  ↓ C++ 代码生成
C++ 源码 (.cpp, .h)
  ↓ g++（C++20）
  ↓ 目标文件 (.o)
  ↓ 链接
本地二进制（可执行文件 / .so）
```

### 命令

| 命令 | 描述 |
|------|------|
| `miva init <name>` | 初始化新项目 |
| `miva build` | 构建项目 |
| `miva run` | 构建并运行 |
| `miva clean` | 清理构建产物 |
| `miva sin-build <file>` | 编译单个源文件 |
| `miva sin-run <file>` | 编译并运行单个源文件 |
| `miva test <file>` | 运行测试文件 |
| `miva get <url>` | 安装依赖 |
| `miva dep` | 显示依赖图 |

选项：
- `--release` —— 发布模式（优化开启，`-O2`）
- `--verbose` —— 详细输出

### 输出结构

```
build/
├── debug/
│   └── <project_name>    # 调试可执行文件
└── release/
    └── <project_name>    # 发布可执行文件

build/debug/cache/
├── src/
│   ├── main.miva.cpp     # 生成的 C++ 源文件
│   ├── main.miva.h       # 生成的 C++ 头文件（导出）
│   ├── main.miva.o       # 编译后的目标文件
│   └── main.miva.sha256  # 源文件哈希（用于缓存判断）
├── std/src/
│   ├── str.miva.cpp
│   ├── str.miva.o
│   └── ...
└── ...
```

---

## 错误码

### 语义错误

| 码 | 描述 |
|-----|------|
| E0001 | 使用了已移动的值（use of moved value） |
| E0002 | 不能移动 ref 参数 / 不能给不可变变量赋值 |
| E0004 | 函数或结构体重复定义 |
| E0005 | 模块声明必须在顶部 / 只能有一个模块 / 重复模块 |
| E0007 | 变量未找到 |
| E0009 | 在 safe 函数中调用 unsafe 函数 / 未知函数 |
| E0010 | 在 safe 函数中解引用指针 |
| E0011 | choose 表达式必须包含 otherwise 分支 |
| E0013 | 无效的魔法指令 |

### 类型错误

| 码 | 描述 |
|-----|------|
| E0014 | 类型不匹配 / 需要值的地方出现了 void |
| E0016 | 函数参数数量或类型不匹配 |
| E0017 | 返回类型不匹配 |
| E0018 | 结构体字面量错误（未知结构体/字段类型错误/缺少字段） |
| E0019 | 结构体中未知字段 |
| E0021 | 无效的类型转换 |
| E0022 | let 声明或赋值中的类型不匹配 |
| E0024 | 数组元素类型必须一致 |
| E0026 | for-each 循环的范围必须是数组 |

---

## 警告码

| 码 | 描述 |
|-----|------|
| W0001 | 命名约定违规（函数/变量名非 snake_case，模块名非小写） |
| W0002 | 使用了已弃用的函数（建议使用标准库替代品） |
| W0003 | 无效的引言注释（intro comment annotation） |
| W0004 | 使用了已弃用的关键字（`c` → 使用 `inline` 替代） |

警告可通过魔法指令控制：

```miva
/! warning_off W0001    // 抑制命名警告
/! warning_err W0002    // 将弃用警告视为错误
```

---

## 已弃用函数

| 函数 | 替代品 |
|------|--------|
| `prints` | 宏 `prints!` |
| `printlns` | 宏 `printlns!` |
| `string_concat` | `std.str.concat` |
| `string_parse` | `std.str.parse_int` |
| `string_length` | `std.str.len` |
| `string_make` | `std.str.make` |
| `ptr_alloc` | `std.mem.alloc` |
| `ptr_realloc` | `std.mem.realloc` |
| `ptr_free` | `std.mem.free` |

## 术语表

- **AST** —— 抽象语法树（Abstract Syntax Tree），源代码结构的内部表示
- **FFI** —— 外部函数接口（Foreign Function Interface），调用 C/C++ 函数的机制
- **Move（移动）** —— 转移值的所有权，使源变量失效
- **Clone（克隆）** —— 显式复制值，源变量保持有效
- **Ref（引用）** —— 借用的值引用（按常量引用传递）
- **Own（拥有）** —— 拥有的参数（按值传递，所有权转移）
- **Box（盒子）** —— 堆分配的值，具有自动生命周期管理
- **Magical（魔法指令）** —— 控制编译器警告、发布模式等的指令
- **Intro（引言）** —— 记录下一个定义的安全性/用途的注解注释
- **Sir / sin-** —— 单文件编译（无需项目配置）
