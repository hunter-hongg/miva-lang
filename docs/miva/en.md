# Miva Programming Language

Miva is a compiled systems programming language that transpiles to C++ and compiles to native binaries via g++. It features a strong static type system, generic programming, pattern matching, move semantics, a safety system, macros, and zero-cost C FFI.

## Table of Contents

- [Overview](#overview)
- [Getting Started](#getting-started)
- [Project Configuration](#project-configuration)
- [Comments](#comments)
- [Module System](#module-system)
- [Definitions](#definitions)
- [Types](#types)
- [Variables](#variables)
- [Expressions](#expressions)
- [Statements](#statements)
- [Control Flow](#control-flow)
- [Functions](#functions)
- [Structs](#structs)
- [Generics](#generics)
- [Safety Levels](#safety-levels)
- [Move Semantics & Ownership](#move-semantics--ownership)
- [Method Call Sugar](#method-call-sugar)
- [Macros](#macros)
- [Built-in Functions](#built-in-functions)
- [Standard Library](#standard-library)
- [C FFI (Foreign Function Interface)](#c-ffi-foreign-function-interface)
- [Operator Overloading](#operator-overloading)
- [Compiler Pipeline & Commands](#compiler-pipeline--commands)
- [Error Codes](#error-codes)
- [Warning Codes](#warning-codes)

---

## Overview

Miva is a full-stack programming language compiler:

1. **Frontend** (`miva-frontend-rs`) — lexes and parses `.miva` source files, producing a JSON AST.
2. **Compiler** (`miva`) — loads the JSON AST, performs macro expansion, semantic analysis, type checking, and generates C++ code.
3. **Backend** (`g++`) — compiles the emitted C++ to native machine code (`.o` files) and links them into an executable or shared library.

The compilation pipeline:

```
.miva source → Lexer/Parser → JSON AST
  → Macro Expansion → Symbol Table → Semantic Analysis
  → Type Checking → C++ Codegen → g++ → Native Binary
```

---

## Getting Started

### Installation

```bash
# Clone the repository
git clone <repo-url>
cd miva-lang

# Build the frontend and compiler
./build.sh --release
```

After building, add both `miva-frontend-rs/target/release/miva-frontend` and `miva/target/release/miva` to your `PATH`.

### Creating a New Project

```bash
# Initialize a new project named "myapp"
miva init myapp
```

This creates the following structure:

```
myapp/
├── miva.toml      # Project configuration
└── src/
    └── main.miva  # Entry point
```

### Building and Running

```bash
# Build the project
miva build --release

# Build and run
miva run --release

# Compile a single file (quick test)
miva sin-build path/to/file.miva
miva sin-run path/to/file.miva

# Clean build artifacts
miva clean

# Run tests
miva test <test_file.miva>
```

### Hello, World!

```miva
module main;

main = () => {
  println("Hello, World");
}
```

Build and run:

```bash
miva build --release
miva run --release
```

---

## Project Configuration

Every Miva project must have a `miva.toml` in its root directory:

```toml
[project]
name = "myapp"
type = "exe"         # "exe" for executable, "lib" for shared library
version = "0.1.0"

[dependencies]
std = "0.1.0"        # Standard library dependency
```

### Project Types

- **`exe`** — compiles to a native executable with a `main()` entry point.
- **`lib`** — compiles to a shared library (`.so`), with `-fPIC` and `-shared` flags. Uses `src/lib.miva` as entry.

### Dependencies

Dependencies are fetched from the standard library path. The standard library is bundled as `std-0.1.0`:

```toml
[dependencies]
std = "0.1.0"
```

Dependencies from GitHub can be installed with:

```bash
miva get <github-url>
```

---

## Comments

Miva supports three kinds of comments:

```miva
// Single-line comment

/*
 * Multi-line block comment (nesting supported)
 * /* Nesting works */
 */

/! Magical directive (controls compiler behavior)
```

### Magical Directives

```miva
/! warning_off W0001    // Suppress warning W0001
/! warning_err W0002    // Treat warning W0002 as an error
/! release always       // Mark as release-only
/! mangle name          // Custom name mangling
```

### Intro Comments (Annotations)

```miva
@ unsafe: performs raw memory operations
@ usage: used as internal helper
@ param: x is the input value
@ impl: trait implementation for struct
@ trusted: safe wrapper around unsafe code
```

Intro comments annotate the next definition and are validated for correctness (e.g., `unsafe` annotation is only valid before `unsafe` functions, `usage` before any definition, etc.).

---

## Module System

Every Miva file must declare exactly one module:

```miva
module main;          // Simple module
module std.io;        // Namespaced module (creates mvp_std::io in C++)
module my.app.utils;  // Deep namespace
```

The module declaration **must** appear at the top of the file before any other definitions.

### Imports

```miva
// Basic import
import "std/str";

// Import with namespace alias
import "std/io" as io;

// Import and bring into current namespace
import "std/io" as .;

// Import C header (generates #include <stdio.h>)
import "c:stdio.h";
```

Import resolution:
- `proj_name/path` — project-internal: resolves to `src/path.miva`
- `std/path` — standard library: resolves to standard library include directory
- `library/path` — external dependency
- `c:header.h` — C header (generates `#include <header.h>`)

### Exports

```miva
export my_function;
export my_struct;
```

Exported symbols are visible to other modules that import this file. Generic functions are emitted as C++ templates in the header file; non-generic functions are declared in the header and defined in the source file.

---

## Definitions

### Functions

```miva
// Simple function (no return value)
greet = () => {
  println("Hello!");
}

// Function with parameters and return type
add = (a: int, b: int): int => {
  return a + b;
}

// Single-expression function (no braces needed)
double = (x: int): int => x * 2

// Void return (no return type specified)
log = (msg: string) => {
  prints(msg);
  print("\n");
}

// Function with ref (borrow) parameter
print_len = (ref s: string) => {
  printlns!(string_length(s));
}
```

### Structs

```miva
// Simple struct
Point = struct {
  x: int,
  y: int,
}

// Empty struct
Empty = struct {}

// Struct with generic type parameters
Box[T] = struct {
  value: T,
}

Pair[T, U] = struct {
  first: T,
  second: U,
}
```

### Struct Literals

```miva
let p Point = struct Point { x = 10, y = 20 };
let b Box[int] = struct Box[int] { value = 42 };
```

### Tests

```miva
test test_name = (): int => {
  assert!(some_condition);
  0;
}
```

Tests are compiled separately as test executables. They must return `int`.

---

## Types

### Primitive Types

| Type | Description | C++ Mapping |
|------|-------------|-------------|
| `int` | Signed integer | `mvp_builtin_int` |
| `bool` | Boolean | `mvp_builtin_boolean` |
| `float32` | 32-bit float | `mvp_builtin_float` |
| `float64` | 64-bit float | `mvp_builtin_float` |
| `char` | Character (byte) | `mvp_builtin_byte` |
| `string` | String | `mvp_builtin_string` |

### Compound Types

| Type | Description | C++ Mapping |
|------|-------------|-------------|
| `array<T>` | Array/Vector of T | `std::vector<T>` |
| `ptr<T>` | Pointer to T | `T*` |
| `box<T>` | Heap-allocated box of T | `mvp_builtin_box<T>` |
| `ptrany` | Void pointer | `mvp_builtin_ptrany` |
| `null` | Void/no value | `void` |

### Struct Types

Struct types are referenced by their name, optionally with generic type arguments:

```miva
let p Point;
let b Box[int];
let pair Pair[int, string];
```

---

## Variables

### Type-Inferred Variable

```miva
// Immutable variable with type inference
x := 42;

// Mutable variable with type inference
mut count := 0;

// Type-inferred variables are immutable by default
```

### Explicitly Typed Variable

```miva
let x int = 42;
let name string = "Miva";
let p Point = struct Point { x = 1, y = 2 };
```

### Assignment

```miva
mut x := 10;
x = 20;     // OK: x is mutable

// Error: cannot assign to immutable variable
y := 10;
y = 20;     // Compile error
```

### Move and Clone

```miva
// Move ownership
move x;                // x is moved, cannot be used afterwards

// Clone (copy) value
clone x;               // x remains valid
```

Primitive types (int, bool, float32, float64, char) are copy types and don't require explicit `clone`. Structs containing only primitive fields are also copy types. Strings, arrays, pointers, and boxes are move types.

---

## Expressions

### Literals

```miva
42            // int
3.14          // float64
true          // bool
false         // bool
'a'           // char
"hello"       // string
"""           // multi-line string literal
line one
line two
"""           // (content between """ markers)
```

### Binary Operators

| Operator | Description | Operand Types |
|----------|-------------|---------------|
| `+` | Addition / String concatenation | int, float32, float64, string |
| `-` | Subtraction | int, float32, float64 |
| `*` | Multiplication | int, float32, float64 |
| `==` | Equality | All comparable types |
| `!=` | Inequality | All comparable types |

Operator precedence (low to high):
1. `==`, `!=`
2. `+`, `-`
3. `*`

### Unary Operators

```miva
addr x     // Address-of: returns ptr<T>
deref p   // Dereference: requires ptr<T>
```

### Cast Expressions

```miva
x as int           // Cast to int
y as float64       // Cast to float64
c as char          // Cast to char
```

Valid casts:
- `int ↔ float32`, `int ↔ float64`, `float32 ↔ float64`
- `int ↔ char`
- `bool → int`
- Same-type cast (identity)

### If Expression

```miva
// If without else (returns void/null)
if (condition) {
  do_something();
};

// If-else (both branches must have same type)
result := if (condition) {
  10
} else {
  20
};
```

`if` is an expression that returns a value. When both branches return values, they must have the same type. Without an `else` branch, the expression yields `null`.

### Choose (Pattern Matching)

```miva
choose (x) {
  when (1) { println("one"); }
  when (2) { println("two"); }
  otherwise { println("other"); }
};
```

- The variable being matched and the `when` values must have the same type.
- All branches must have the same type.
- `otherwise` is **required** — compiler error E0011 if omitted.

### Blocks

```miva
// Block expression — returns the last expression
result := {
  let x int = 10;
  let y int = 20;
  x + y         // ← block result
};

// Block with explicit return
{
  prints("hello");
  prints(" ");
  prints("world");
}     // ← void block
```

---

## Statements

### Let Statements

```miva
// Type-inferred (immutable)
name := value;

// Type-inferred (mutable)
mut name := value;

// Explicitly typed
let name Type = value;
```

### Expression Statements

Any expression followed by `;` is a statement:

```miva
println("test");
x + 1;
```

### Return Statement

```miva
return x + 1;
return;       // Return void
```

### Assignment Statement

```miva
// Variable must be mutable
x = x + 1;
```

### Empty Statement

```miva
;   // No-op
```

---

## Control Flow

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

Note: the closing `}` is followed by `;` when used as a statement.

### While Loop

```miva
while (condition) {
  ...
};
```

### Infinite Loop

```miva
loop {
  ...
};
```

### For-In Loop

```miva
for i in (range(10)) {
  printlns!(i);
};
```

The for-in loop iterates over an array. The loop variable has the element type of the array.

---

## Functions

### Function Syntax

```miva
// name = (params): return_type => expression
add = (a: int, b: int): int => a + b

// Multi-statement function (block body)
factorial = (n: int): int => {
  if (n <= 1) {
    return 1;
  } else {
    return n * factorial(n - 1);
  };
}
```

### Parameters

```miva
// Own parameter (ownership transferred)
foo = (x: int, y: string) => { ... }

// Ref parameter (borrowed, const reference)
bar = (ref x: int, ref s: string) => { ... }
```

- **`ref`** parameters are passed as `const&` in C++ — no ownership transfer.
- **`own`** parameters (default) receive ownership; the parameter can be moved.

### Calling Functions

```miva
// Regular call
add(3, 4);

// Call with explicit type arguments (generic functions)
identity[int](42);
mk_pair[int, string](1, "one");

// Method call syntax (desugars to function call)
x.twice()                   // → twice(x)
n.add(5)                    // → add(n, 5)
n.add(3).add(4)             // → add(add(n, 3), 4)

// Method call with type arguments
p.first[int, string]()      // → first[int, string](p)
```

Method call syntax automatically inserts the receiver as the first argument.

### Generic Functions

```miva
// Single type parameter
identity[T] = (x: T): T => x

// Multiple type parameters
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// Calling with explicit type arguments
let p Pair[int, string] = mk_pair[int, string](1, "one");
```

Type parameters can often be inferred from arguments:

```miva
let x = identity[int](42);   // Explicit
let y = identity(42);        // Inferred (if the compiler can deduce T)
```

### Nested Functions (Functions as Top-Level Definitions Only)

All function definitions are top-level. There are no nested function expressions in blocks.

### Recursion

Functions can be recursive (calling themselves). Full tail-call optimization is not guaranteed.

---

## Generics

Miva supports generics on structs and functions.

### Generic Structs

```miva
Box[T] = struct {
  value: T,
}

Pair[T, U] = struct {
  first: T,
  second: U,
}

// Struct with empty type params
Empty[T] = struct {}
```

### Generic Functions

```miva
// Generic function using generic type
mk_box[T] = (x: T): Box[T] => struct Box[T] { value = x }

// Multiple type params in function
mk_pair[T, U] = (a: T, b: U): Pair[T, U] => struct Pair[T, U] { first = a, second = b }

// Nested generics
mk_nested[T, U] = (a: T, b: U): Box[Pair[T, U]] => struct Box[Pair[T, U]] { value = mk_pair[T, U](a, b) }
```

Type arguments use bracket syntax: `func[T, U](args)`.

Generic functions are compiled to C++ templates. Generic structs become C++ template structs. Both must be fully defined in headers, so exported generic functions are emitted inline.

---

## Safety Levels

Miva provides three safety levels for functions:

### Safe (Default)

```miva
// All functions are safe by default
main = () => {
  println("Hello");
}
```

Safe functions **cannot**:
- Call `unsafe` functions
- Dereference pointers (`deref` expression)
- Use raw pointer builtins (`ptr_alloc`, `ptr_realloc`, `ptr_free`, `ptr_set`)

### Unsafe

```miva
unsafe dangerous_op = (p: ptr<int>) => {
  deref p;
}
```

Unsafe functions can:
- Call other unsafe functions
- Use `deref` and `addr`
- Use raw pointer builtins

### Trusted

```miva
trusted safe_wrapper = (p: ptr<int>): int => {
  return deref p;
}
```

Trusted functions can perform unsafe operations but are callable from safe code. They serve as safe abstractions around unsafe primitives.

### Safety Restriction Flow

```
safe function → can call: safe, trusted (NOT: unsafe)
unsafe function → can call: safe, unsafe, trusted
trusted function → can call: safe, unsafe, trusted
```

---

## Move Semantics & Ownership

Miva uses a Rust-inspired ownership system with move semantics.

### Move

```miva
// Move transfers ownership
main = () => {
  s := "hello";           // s owns the string
  consume(move s);        // s is moved; s becomes invalid
  printlns!(s);           // Error: use of moved value 's' (E0001)
}
```

### Clone

```miva
main = () => {
  s := "hello";
  consume(clone s);       // s is cloned; s remains valid
  printlns!(s);           // OK
}
```

### Copy Types

Primitive types (`int`, `bool`, `float32`, `float64`, `char`) and structs composed entirely of copy types are automatically copied — no explicit `clone` needed.

### Ref Parameters

`ref` parameters borrow (not move) the value. They cannot be moved:

```miva
bar = (ref x: int) => {
  move x;                // Error: cannot move ref parameter (E0002)
}
```

### Assigning After Move

Assigning to a mutable variable resets its state, making it valid again:

```miva
mut x := 42;
consume(move x);         // x is moved
x = 99;                  // x is valid again
```

### If/Choose Branch Merging

After an `if` expression, if a variable is moved in **all** branches, it's considered moved after the `if`. If moved in only one branch, it remains valid (because both branches must have moved it).

---

## Method Call Sugar

Method call syntax `receiver.method(args...)` automatically desugars to `method(receiver, args...)` at compile time.

```miva
twice = (x: int): int => x * 2
add = (a: int, b: int): int => a + b

main = () => {
  n := 10;

  // Zero extra args: n.twice() → twice(n)
  prints(n.twice())

  // One extra arg: n.add(5) → add(n, 5)
  prints(n.add(5))

  // Chaining: n.twice().add(5) → add(twice(n), 5)
  prints(n.twice().add(5))

  // Chaining with nested method call:
  // n.add(3).add(n.add(4)) → add(add(n, 3), add(n, 4))
  prints(n.add(3).add(n.add(4)))
}
```

Method call syntax supports generic type arguments:

```miva
p.first[int, string]()          // → first[int, string](p)
```

---

## Macros

Miva has two kinds of macros: **built-in macros** and **user-defined macros**.

### Built-in Macros

#### `prints!(...)`

Prints multiple values separated by spaces. Automatically converts values to strings.

```miva
prints!("hello", 42, true);    // Output: "hello 42 true "
```

Expands to:

```miva
let s string = "";
s = s + string_from("hello") + " ";
s = s + string_from(42) + " ";
s = s + string_from(true) + " ";
print(s);
```

#### `printlns!(...)`

Same as `prints!` but adds a trailing newline.

```miva
printlns!(1, 2, 3);    // Output: "1 2 3\n"
```

#### `assert!(expr)`

If the expression evaluates to `false`, panics with "Assertion failed".

```miva
assert!(x == 42);
```

Expands to:

```miva
if (x == 42 == false) {
  panic("Assertion failed");
} else {};
```

#### `include_str!("path")`

Reads a file at compile time and embeds its contents as a string literal.

```miva
let contents string = include_str!("data.txt");
```

### User-Defined Macros

```miva
// Macro definition
macro double = ($x: int) => $x + $x

macro greet = ($name: string) => {
  prints!("Hello, ");
  prints!($name);
  prints!("!\n");
}

// Macro call
double!(5);        // Expands to: 5 + 5
greet!("World");   // Expands to greeting block
```

Macro syntax:
- Parameters are prefixed with `$`: `$name`, `$x`, etc.
- Parameters have explicit types: `($x: int, $y: string)`
- The macro body uses `=>` syntax, just like functions
- Inside the body, `$param` references become `EMacroVar` nodes that are substituted with the argument expressions at expansion time

Macros are expanded **before** semantic analysis and type checking. This means macros can work with any expression type, and errors are reported on the expanded code.

### Macro Scoping

Macros are collected project-wide **before** compilation. A macro defined in any file in the project is available to all other files. `DMacro` definitions are removed from the AST after expansion.

### Nested Macros

Macros can call other macros (including nested built-in macros):

```miva
macro assert_eq = ($got: int, $expected: int) => {
  if ($got != $expected) {
    prints!("FAIL: expected ");
    printlns!($expected);
  } else {};
}
```

---

## Built-in Functions

### Output Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `print` | `(s: string)` | Print string |
| `prints` | `(s: string)` | Print string (deprecated, use `prints!`) |
| `println` | `(s: string)` | Print string with newline |
| `printlns` | `(s: string)` | Print string with newline (deprecated, use `printlns!`) |
| `error` | `(s: string)` | Print to stderr |
| `errors` | `(s: string)` | Print to stderr (deprecated) |
| `errorln` | `(s: string)` | Print to stderr with newline |
| `errorlns` | `(s: string)` | Print to stderr with newline (deprecated) |

### Control Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `exit` | `(code: int)` | Exit process with code |
| `abort` | `()` | Abort process |
| `panic` | `(msg: string)` | Panic with message (abort with message) |

### String Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `string_concat` | `(a: string, b: string): string` | Concatenate strings (deprecated) |
| `string_parse` | `(s: string): int` | Parse string to int (deprecated) |
| `string_length` | `(s: string): int` | Get string length (deprecated) |
| `string_make` | `(s: string, n: int): string` | Make string (deprecated) |
| `string_from` | `(x: T): string` | Convert value to string |

### Box Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `box_new` | `(x: T): box<T>` | Create a new box |
| `box_deref` | `(b: box<T>): T` | Dereference a box |

### Range Function

| Function | Signature | Description |
|----------|-----------|-------------|
| `range` | `(n: int): array<int>` | Create array `[0, 1, ..., n-1]` |

### Unsafe Pointer Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ptr_alloc` | `(size: int): ptrany` | Allocate memory |
| `ptr_realloc` | `(p: ptrany, size: int): ptrany` | Reallocate memory |
| `ptr_free` | `(p: ptrany)` | Free memory |
| `ptr_set` | `(p: ptrany, v: int)` | Write to pointer |

All pointer functions are **unsafe** and can only be used in `unsafe` or `trusted` functions.

### FFI (Foreign Function Interface)

Functions prefixed with `ffi.` map to C++ namespaced calls:

```miva
ffi.some_c_func(a, b);    // Compiles to: some_c_func(a, b)
ffi.ns.func(args);        // Compiles to: ns::func(args)
```

There is no automatic C binding generation; the C function must be linked manually or via `c unsafe`.

---

## Standard Library

The Miva standard library (`std-0.1.0`) provides:

### `std.str` — String Utilities

```miva
import "std/str";

std.str.concat(ref a, ref b)      // String concatenation
std.str.parse_int(ref s)           // String to int parsing
std.str.len(ref s)                 // String length
std.str.make(ref s, ref size)     // String repeat
std.str.from[T](x)                // Value to string (generic)
```

Type parameters are exported for `from[T]`, making it available across modules.

### `std.io` — Colored I/O

```miva
import "std/io";

std.io.cprint(ref x, ref color)     // Print with color
std.io.cprintln(ref x, ref color)   // Print line with color
std.io.eprint(ref x, ref color)     // Error print with color
std.io.eprintln(ref x, ref color)   // Error print line with color
```

### `std.mem` — Memory Management

```miva
import "std/mem";

std.mem.alloc(ref size)             // Allocate (ptrany)
std.mem.realloc(ref p, size)        // Reallocate
std.mem.free(ref p)                 // Free
```

### `std.term` — Terminal Color Codes

```miva
import "std/term";

std.term.color_null()     // Reset color
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

These modules exist as stubs for future functionality.

---

## C FFI (Foreign Function Interface)

Miva allows embedding raw C++ code via the `c unsafe` function syntax:

```miva
// Using "c" keyword (deprecated, generates W0004 warning)
c unsafe puts = (s: string): int => {
  return puts(s);
}

// Using "inline" keyword (preferred)
inline unsafe printf_wrapper = (fmt: string): int => {
  return printf("%s", fmt);
}
```

The C++ code between `{ }` is inserted directly into the generated C++ translation unit.

### Raw Braceless C Function (String Body)

```miva
c unsafe custom_fn = (x: int): int => "return x * 2;"
```

This avoids the brace-delimited raw block and uses a string literal for the body.

---

## Operator Overloading

Operator overloading is supported via the `impl` block:

```miva
impl Point {
  op_add my_add,    // my_add(a, b) → a + b
  op_sub my_sub,    // my_sub(a, b) → a - b
  op_mul my_mul,    // my_mul(a, b) → a * b
  op_eq my_eq,      // my_eq(a, b) → a == b
  op_neq my_neq,    // my_neq(a, b) → a != b
}
```

This generates C++ `operator+`, `operator-`, `operator*`, `operator==`, and `operator!=` functions for the struct type, delegating to the named functions.

---

## Compiler Pipeline & Commands

### Pipeline

```
Source (.miva)
  ↓ Lexer & Parser (miva-frontend-rs)
JSON AST
  ↓ Collect macros project-wide
  ↓ Macro expansion (built-in + user-defined)
  ↓ Symbol table construction
  ↓ Semantic analysis
    • Variable resolution
    • Move semantics
    • Safety enforcement
    • Module/import validation
  ↓ Type checking
    • Type inference
    • Generic type substitution
    • Type consistency verification
  ↓ Warning generation & filtering
  ↓ C++ code generation
C++ source (.cpp, .h)
  ↓ g++ (C++20)
  ↓ Object files (.o)
  ↓ Linking
Native binary (.exe / .so)
```

### Commands

| Command | Description |
|---------|-------------|
| `miva init <name>` | Initialize a new project |
| `miva build` | Build the project |
| `miva run` | Build and run |
| `miva clean` | Clean build artifacts |
| `miva sin-build <file>` | Compile a single file |
| `miva sin-run <file>` | Compile and run a single file |
| `miva test <file>` | Run test files |
| `miva get <url>` | Install a dependency |
| `miva dep` | Show dependency graph |

Options:
- `--release` — Release mode (optimized, `-O2`)
- `--verbose` — Verbose output

### Output Structure

```
build/
├── debug/
│   └── <project_name>    # Debug executable
└── release/
    └── <project_name>    # Release executable

build/debug/cache/
├── src/
│   ├── main.miva.cpp     # Generated C++ source
│   ├── main.miva.h       # Generated C++ header (exports)
│   ├── main.miva.o       # Compiled object
│   └── main.miva.sha256  # Source hash for caching
├── std/src/
│   ├── str.miva.cpp
│   ├── str.miva.o
│   └── ...
└── ...
```

---

## Error Codes

### Semantic Errors

| Code | Description |
|------|-------------|
| E0001 | Use of moved value |
| E0002 | Cannot move ref parameter / assign to immutable variable |
| E0004 | Duplicate function or struct definition |
| E0005 | Module declaration must be at top / only one module / duplicate module |
| E0007 | Variable not found |
| E0009 | Cannot call unsafe function from safe / unknown function |
| E0010 | Cannot dereference pointer in safe function |
| E0011 | Choose expression must have an otherwise branch |
| E0013 | Invalid magical comment |

### Type Errors

| Code | Description |
|------|-------------|
| E0014 | Type mismatch / void value where non-void expected |
| E0016 | Function argument count or type mismatch |
| E0017 | Return type mismatch |
| E0018 | Struct literal error (unknown struct / wrong field type / missing field) |
| E0019 | Unknown field in struct |
| E0021 | Invalid cast |
| E0022 | Type mismatch in let declaration or assignment |
| E0024 | All array elements must have the same type |
| E0026 | For-each loop range must be an array |

---

## Warning Codes

| Code | Description |
|------|-------------|
| W0001 | Naming convention violation (non-snake_case function/variable, non-lowercase module) |
| W0002 | Deprecated function usage (use std library replacements) |
| W0003 | Invalid intro comment annotation |
| W0004 | Deprecated keyword usage (`c` → use `inline` instead) |

Warnings can be controlled via magical directives:

```miva
/! warning_off W0001    // Suppress naming warnings
/! warning_err W0002    // Treat deprecation warnings as errors
```

---

## Deprecated Functions

| Function | Replacement |
|----------|-------------|
| `prints` | Macro `prints!` |
| `printlns` | Macro `printlns!` |
| `string_concat` | `std.str.concat` |
| `string_parse` | `std.str.parse_int` |
| `string_length` | `std.str.len` |
| `string_make` | `std.str.make` |
| `ptr_alloc` | `std.mem.alloc` |
| `ptr_realloc` | `std.mem.realloc` |
| `ptr_free` | `std.mem.free` |

## Glossary

- **AST** — Abstract Syntax Tree, internal representation of source code structure
- **FFI** — Foreign Function Interface, mechanism to call C/C++ functions
- **Move** — Transfer of ownership of a value, invalidating the source
- **Clone** — Explicit copy of a value, source remains valid
- **Ref** — Borrowed reference to a value (pass-by-const-reference)
- **Own** — Owned parameter (pass-by-value, ownership transferred)
- **Box** — Heap-allocated value with automatic lifetime management
- **Magical** — Compiler directive controlling warnings, release mode, etc.
- **Intro** — Annotation comment documenting safety/usage of the next definition
- **Sir (sin-)** — Single file compilation (no project setup needed)
