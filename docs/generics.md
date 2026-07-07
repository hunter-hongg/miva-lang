# 泛型（Generics）

## 概述

Miva 的泛型使用方括号 `[T, U]` 语法，编译为 C++ 模板（`template<typename T, typename U>`）。类型参数在定义时声明，在使用时通过显式类型实参或类型推断实例化。

---

## 1. 泛型结构体定义

语法：`StructName[T, U] = struct { ... }`

```miva
// 单个类型参数
Box[T] = struct {
  value: T
}

// 多个类型参数
Pair[T, U] = struct {
  first: T,
  second: U,
}

// 嵌套泛型：类型参数可出现在字段类型中
Wrapper[T] = struct {
  inner: Box[T]
}
```

解析规则（`parser.rs:146-163`）：
- 结构体名后紧跟 `[`，解析逗号分隔的标识符作为类型参数列表
- 类型参数存储为 `Vec<String>`，挂载在 `DStruct { type_params }` 上

---

## 2. 泛型函数定义

语法：`funcName[T, U] = (params): retType => body`

```miva
// 单个类型参数
identity[T] = (x: T): T => x

// 多个类型参数
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// 参数和返回值都使用类型参数
swap[T] = (a: T, b: T): T => a

// 无返回值（返回 unit）
print_box[T] = (b: Box[T]) => printlns!(b.value)
```

解析规则（`parser.rs:146-163`，同结构体）：
- 函数名后紧跟 `[`，解析逗号分隔的标识符作为类型参数列表
- 类型参数存储为 `Vec<String>`，挂载在 `DFunc { type_params }` 上

---

## 3. unsafe / trusted 泛型函数

`unsafe` 和 `trusted` 关键字修饰的函数同样支持泛型。

```miva
unsafe foo[T] = (x: T): T => x
trusted bar[T] = (x: T): T => x
```

解析规则（`parser.rs:237-253`, `parser.rs:261-278`）：
- 与普通泛型函数相同，在函数名后解析 `[TypeParams]`

---

## 4. 类型引用时指定泛型实参

在类型注解中引用泛型结构体时，使用 `Name[T, U]` 语法。

```miva
main = () => {
  let bi Box[int] = mk_box[int](42);
  let bb Box[bool] = mk_box[bool](true);
  let p Pair[int, bool] = mk_pair[int, bool](10, false);
  let bn Box[Pair[int, int]] = mk_nested[int, int](7, 8);
}
```

解析规则（`parser.rs:581-601`）：
- 在 `parse_typ` 中，解析标识符类型后检查 `[`，解析逗号分隔的类型作为 `TStruct { type_args }`

---

## 5. 函数调用时显式指定泛型实参

语法：`funcName[T, U](arg1, arg2)`

```miva
main = () => {
  // 单类型参数调用
  identity[int](42);
  identity[string]("hello");

  // 多类型参数调用
  mk_pair[int, bool](10, false);
  mk_pair[int, string](1, "one");

  // 嵌套泛型调用
  mk_pair[string, Pair[int, int]]("nums", inner);
}
```

解析规则（`parser.rs:759-792`）：
- `parse_call_suffix` 中，遇到 `[` 时先解析类型实参列表，再解析 `(args)` 作为函数调用
- 类型实参存储在 `ECall { type_args }` 中

---

## 6. 结构体字面量指定泛型实参

语法：`struct Name[T, U] { field1 = value1, field2 = value2 }`

```miva
main = () => {
  // 结构体字面量带泛型实参
  let b = struct Box[int] { value = 42 };

  // 在泛型函数体中使用
  mk_box[T] = (x: T): Box[T] => struct Box[T] { value = x }
}
```

解析规则（`parser.rs:1269-1304`）：
- `parse_struct_init_expr` 中，解析类型路径后检查 `[`，解析类型实参列表
- 类型实参存储在 `EStructLit { type_args }` 中

---

## 7. 类型推断

调用泛型函数时，如果未显式指定类型实参，编译器会从参数类型推断。

```miva
infer_demo[T] = (x: T): T => x

main = () => {
  // 从参数 42 推断 T = int
  infer_demo(42);

  // 从参数 "hello" 推断 T = string
  infer_demo("hello");
}
```

实现（`typecheck.rs:89-114`）：
- `infer_type_from_arg` 递归匹配参数类型和类型参数
- 支持数组、指针、Box、结构体嵌套中的类型参数推断
- 多个参数对同一类型参数的类型必须一致，否则报错

---

## 8. 类型参数名称解析

在类型检查阶段，类型参数名在类型注解中被"归一化"为 `TGenericParam`。

```miva
Box[T] = struct { value: T }
```

- 解析阶段：字段 `value` 的类型为 `TStruct { name: "T" }`
- 归一化阶段（`typecheck.rs:8-35`）：当 `"T"` 匹配当前作用域的类型参数列表时，转换为 `TGenericParam { name: "T" }`
- 这解决了类型参数名与同名字符串的歧义

---

## 9. 泛型实参解析（resolve）

类型检查时，将 `TGenericParam` 替换为具体类型。

```miva
identity[int](42)
```

- 建立替换表：`{ "T" -> TInt }`
- `resolve_type`（`typecheck.rs:63-86`）递归替换所有类型参数
- 返回值类型从 `TGenericParam { name: "T" }` 解析为 `TInt`

---

## 10. 跨模块泛型（导出）

泛型定义通过 `export` 导出后，在其他模块中可以使用。

**func.miva**（导出模块）：
```miva
module gd/func;

Pair[T, U] = struct {
  first: T,
  second: U,
}

export Pair;

my_identity[T] = (x: T): T => x
export my_identity;

mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }
export mk_pair;

first[T, U] = (p: Pair[T, U]): T => p.first
export first;

second[T, U] = (p: Pair[T, U]): U => p.second
export second;

swap_pair[T, U] = (p: Pair[T, U]): Pair[U, T] => struct Pair[U, T] { first = p.second, second = p.first }
export swap_pair;
```

**main.miva**（使用模块）：
```miva
module main;
import "gd/func";

main = () => {
  // 使用跨模块泛型
  printlns!(my_identity[int](42));
  printlns!(my_identity[string]("hello"));

  p := mk_pair[int, string](1, "one");
  printlns!(first[int, string](p));
  printlns!(second[int, string](p));

  swapped := swap_pair[int, string](p);
}
```

代码生成（`codegen.rs:840-873`）：
- 由于 C++ 模板要求在实例化点可见完整定义，导出的泛型函数和结构体**必须在头文件中发出完整定义**（而非仅声明）
- `generate_header` 中通过 `find_func_def` / `find_struct_def` 查找定义并生成完整代码

---

## 11. C++ 代码生成映射

| Miva 泛型语法 | C++ 生成结果 |
|---|---|
| `Box[T] = struct { value: T }` | `template<typename T> struct Box { mvp_builtin_box<T> value; };` |
| `identity[T] = (x: T): T => x` | `template<typename T> T identity(T x) { return x; }` |
| `Box[int]`（类型引用） | `Box<mvp_builtin_int>` |
| `identity[int](42)`（调用） | `identity<mvp_builtin_int>(42)` |
| `struct Pair[T, U] { ... }`（字面量） | `Pair<Arg1, Arg2>{ ... }` |

实现位置：
- `cxx_type`（`codegen.rs:56-75`）：`TStruct { type_args }` 输出 `Name<Arg1, Arg2>`，`TGenericParam` 直接输出参数名
- `cxx_struct_def`（`codegen.rs:517-542`）：有 `type_params` 时添加 `template<typename T, ...>` 前缀
- `cxx_normal_func`（`codegen.rs:586-643`）：有 `type_params` 时添加 `template<typename T, ...>` 前缀
- `cxx_call` / `cxx_struct_lit`：类型实参渲染为 C++ 模板实参 `<Arg1, Arg2>`

---

## 12. 语法汇总

| 用途 | 语法 | 示例 |
|---|---|---|
| 泛型结构体定义 | `Name[T, U] = struct { ... }` | `Pair[T, U] = struct { first: T, second: U }` |
| 泛型函数定义 | `name[T, U] = (params): ret => body` | `identity[T] = (x: T): T => x` |
| unsafe 泛型函数 | `unsafe name[T] = ...` | `unsafe foo[T] = (x: T): T => x` |
| trusted 泛型函数 | `trusted name[T] = ...` | `trusted bar[T] = (x: T): T => x` |
| 类型注解中的泛型实参 | `TypeName[T, U]` | `let x Box[int] = ...` |
| 调用时的显式泛型实参 | `funcName[T, U](args)` | `identity[int](42)` |
| 结构体字面量的泛型实参 | `struct Name[T, U] { fields }` | `struct Box[int] { value = 42 }` |
| 嵌套泛型 | `Outer[Inner[T]]` | `Box[Pair[int, int]]` |
| 类型参数重排 | `Func[A, B]` 返回 `Type[B, A]` | `swap_pair[T, U]` 返回 `Pair[U, T]` |

## 13. 当前限制

- 不支持 trait bounds / where 子句 / 类型约束
- 不支持关联类型
- 不支持高阶类型（higher-kinded polymorphism）
- 不支持泛型类型参数的默认值
- 泛型实参必须由调用方显式指定或从参数类型推断，不支持从返回值类型推断
