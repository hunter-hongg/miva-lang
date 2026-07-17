# Miva Programming Language

Miva is a compiled systems programming language. The compiler transpiles Miva source to C++ (the default backend), or to LLVM IR, or to Miva Virtual Machine (MVM) bytecode, and compiles to native binaries or runs directly on the interpreter. It features a strong static type system, generic programming, pattern matching, move semantics, a safety system, macros, and zero-cost C FFI.

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
- [Async](#async)
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
2. **Compiler** (`miva`) — loads the JSON AST, performs macro expansion, semantic analysis, type checking, and generates code for the selected backend.
3. **Backend** — one of three backends (see below) turns the generated artifact into a native binary or runs it on the MVM interpreter.

The compilation pipeline:

```
.miva source → Lexer/Parser → JSON AST
  → Macro Expansion → Symbol Table → Semantic Analysis
  → Type Checking → Codegen (C++ / LLVM IR / MVM bytecode) → Native Binary / MVM
```

### Backends

Miva supports three backends, selectable per-build via the `-b` flag or the `[project] backend` field in `miva.toml`:

| Backend | `-b` value | Output | Notes |
|---------|------------|--------|-------|
| **C++** (`cxx`) | `cxx` | Native executable / `.so` | Default. Emits C++20 compiled by `g++`. |
| **LLVM** (`llvm`) | `llvm` | Native executable / `.so` | Emits LLVM IR compiled via `llc` + `g++` linker. |
| **MVM** (`mvm`) | `mvm` | `.mvm` bytecode | Emits Miva Virtual Machine bytecode, run by the `mvm` interpreter (no native linker needed). |

The `cxx` and `llvm` backends both produce native binaries. The `mvm` backend produces portable bytecode executed by the bundled `mvm` interpreter (`miva-vm`), which is useful for quick iteration and cross-platform runs.

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
# Initialize a new binary project named "myapp"
miva init myapp -t bin

# Initialize a shared-library project
miva init mylib -t lib
```

`-t` selects the project type: `bin` (executable) or `lib` (shared library). This creates the following structure:

```
myapp/
├── miva.toml      # Project configuration
└── src/
    └── main.miva  # Entry point (src/lib.miva for lib projects)
```

### Building and Running

```bash
# Build the project (default cxx backend)
miva build --release

# Build and run
miva run --release

# Build/run with a specific backend
miva run -b llvm          # LLVM backend
miva run -b mvm           # MVM backend (alias: --mvm)
miva build -b mvm         # emit .mvm bytecode

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
type = "bin"         # "bin" for executable, "lib" for shared library
version = "0.1.0"
backend = "cxx"      # optional: cxx (default), llvm, or mvm

[env]

[scripts]
dev = "miva run -b mvm"
release = "miva build -b llvm --release"

[dependencies]
std = "0.1.2"        # Standard library dependency
```

### Project Types

- **`bin`** — compiles to a native executable with a `main()` entry point (uses `src/main.miva`).
- **`lib`** — compiles to a shared library (`.so`), with `-fPIC` and `-shared` flags. Uses `src/lib.miva` as entry.

### Backend Selection

The backend is chosen from, in priority order: the `-b` / `--mvm` command-line flag, then the `[project] backend` field in `miva.toml` (default `cxx`). See [Compiler Pipeline & Commands](#compiler-pipeline--commands) for backend details.

### Scripts

The `[scripts]` section defines custom commands runnable as `miva <name>`. Built-in command names (`init`, `build`, `run`, `clean`, `sin-build`, `sin-run`, `get`, `dep`, `test`, `reinit`) always take precedence over scripts.

### Dependencies

Dependencies are fetched from the standard library path. The standard library is bundled as `std-0.1.2`:

```toml
[dependencies]
std = "0.1.2"
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

## Async

Miva provides a thread-based async model: a function declared with the `async` keyword is launched on its own OS thread when called and immediately returns a `future[T]` handle; the result is retrieved later with `.await()` or `await(...)`, which blocks and joins the task.

### Syntax

An `async` function must annotate its return type as `future[T]` — the element type `T` is the task's result type:

```miva
async square = (x: int): future[int] => {
  return x * x;
}
```

Calling an `async` function does not block: it immediately returns a `future[int]`. Calling `.await()` on that handle (or `await(handle)`) waits for the thread to finish and yields the inner `int`.

### Example

From `examples/async/src/main.miva`:

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
  f := square(5);                       // f is immediately a future[int]; task runs in background
  g := greet("miva");                   // same
  a := square(3).await();               // blocks until square(3) finishes
  b := square(4).await();
  printlns!(f.await(), g.await(), a, b);
  printlns!(combine(7).await());
  printlns!(add(square(2).await(), square(3).await()).await());
}
```

Key points:

- Calling an `async` function **returns immediately** with a `future[T]`; the task runs concurrently on a background thread.
- `.await()` is method-call sugar for `await(...)`; the two are equivalent.
- `.await()` can be chained (as inside `combine`) to compose multiple async tasks.
- Calling `await(...)` on a **non**-future value is an identity operation — it returns the value as-is, so `await` can safely wrap any expression.

### Types

`future[T]` is a built-in composite type. An `async` function's declared return type must be of the form `future[T]`, otherwise type checking rejects it ("async function must return future[T]"). The type argument `T` may be any Miva type, including `string` and structs.

| Type | Description | C++ mapping |
|------|-------------|-------------|
| `future[T]` | Handle to a task of `T` | `mvp_future<T>` |

### Backend implementations

- **C++ (`cxx`)** — an `async` function compiles to a wrapper that returns `mvp_future<T>`; the body is captured into a lambda and run via `mvp_async_spawn` over `std::async(std::launch::async)` on a `std::future`. `.await()` maps to `mvp_async_await`, calling `std::future::get()`. A `shared_ptr` keeps the future copyable, so both `let f = task(); f.await()` and `task().await()` work.
- **LLVM (`llvm`)** — calling an `async` function spawns a dedicated OS thread through the runtime bridge `miva_async_spawn` (a `std::thread`-based struct) and returns a task handle (i64); `await(...)` calls `miva_async_await`, which waits via a `std::condition_variable` and joins the thread.
- **MVM (`mvm`)** — the `Call` bytecode spawns, when the target is an `async` function, a fresh `Mvm` instance on a new thread to run that function, pushing a `Value::Future` (holding the result and thread handle). The `await` bytecode (`Opcode::Await`) joins the thread and takes its result.

### Safety & concurrency semantics

- `async` functions are **safe** by default; they may call other safe / trusted functions and are subject to the move/ownership rules. Their parameters are captured by value into the background thread (including `ref` parameters, which are copied to avoid dangling references).
- Async tasks run concurrently on separate threads with the caller; sharing immutable data safely is the programmer's responsibility — Miva does not yet ship language-level locks, so mutual exclusion is provided by the standard library or `inline unsafe` C/C++ code.
- `await` blocks the current thread until the future completes, so awaiting the same handle multiple times is safe and idempotent.

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

The Miva standard library (`std-0.1.2`) provides:

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

### `std.vec` — Vector

```miva
import "std/vec";

std.vec.new[T]()                  // Create an empty vector
std.vec.push[T](ref v, x)         // Append element
std.vec.get[T](ref v, i)          // Get element by index
std.vec.len[T](ref v)             // Number of elements
std.vec.pop[T](ref v)             // Remove and return last element
```

### `std.box` — Boxed Values

```miva
import "std/box";

std.box.new[T](x)                 // Create a boxed value
std.box.get[T](ref b)             // Dereference a box
std.box.set[T](ref b, x)          // Update boxed value
```

### `std.json` — JSON Parsing

```miva
import "std/json";

std.json.parse(ref s)             // Parse string -> ptrany (JSON tree)
std.json.object_get(ref v, i)     // Get object/array element by index
std.json.object_len(ref v)        // Number of keys/elements
std.json.object_key(ref v, i)     // Get object key by index
std.json.object_find(ref v, k)    // Find object value by key
std.json.kind(ref v)              // JSON node kind (bool/number/string/array/object)
std.json.bool(ref v)              // Extract bool value
std.json.number(ref v)            // Extract number value
std.json.string(ref v)            // Extract string value
std.json.stringify(ref v)         // Serialize JSON tree to string
std.json.free(ref v)              // Free the JSON tree
```

### `std.xml` — XML Parsing

```miva
import "std/xml";

std.xml.parse(ref s)              // Parse string -> ptrany (XML tree)
std.xml.kind(ref v)               // Node kind (element/text/comment/cdata/pi)
std.xml.tag(ref v)                // Element tag name
std.xml.attr_count(ref v)         // Number of attributes
std.xml.attr_name(ref v, i)       // Attribute name by index
std.xml.attr_value(ref v, i)      // Attribute value by index
std.xml.attr_find(ref v, k)       // Find attribute value by name
std.xml.child_count(ref v)        // Number of child nodes
std.xml.child_get(ref v, i)       // Child node by index
std.xml.text(ref v)               // Text content of a text node
std.xml.comment(ref v)            // Comment content
std.xml.cdata(ref v)              // CDATA content
std.xml.pi_target(ref v)          // Processing-instruction target
std.xml.pi_data(ref v)            // Processing-instruction data
std.xml.stringify(ref v)          // Serialize XML tree to string
std.xml.free(ref v)              // Free the XML tree
```

### `std.toml` / `std.yaml` — TOML & YAML Parsing

Both expose the same tree API as JSON (parse → `ptrany`, then `object_get`/`object_len`/etc.) and reuse the JSON node representation.

```miva
import "std/toml";
import "std/yaml";

std.toml.parse(ref s)             // Parse TOML string -> ptrany
std.yaml.parse(ref s)             // Parse YAML string -> ptrany
```

---

## C FFI (Foreign Function Interface)

Miva allows embedding raw C++ code via the `inline unsafe` function syntax:

```miva
// Using "inline" keyword (preferred)
inline unsafe printf_wrapper = (fmt: string): int => {
  return printf("%s", fmt);
}

// Using "c" keyword (deprecated, generates W0004 warning)
c unsafe puts = (s: string): int => {
  return puts(s);
}
```

The C++ code between `{ }` is inserted directly into the generated C++ translation unit. On the `llvm` and `mvm` backends, `inline`/`c` raw blocks are not available and such functions must be provided externally or omitted.

### Raw Braceless C Function (String Body)

```miva
inline unsafe custom_fn = (x: int): int => "return x * 2;"
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
  ↓ Code generation (C++ / LLVM IR / MVM bytecode)
C++ source (.cpp, .h)        LLVM IR (.ll)          MVM bytecode (.mvm)
  ↓ g++ (C++20)               ↓ llc + g++ linker         ↓ mvm interpreter
  ↓ Object files (.o)                                    (no native linker needed)
  ↓ Linking
Native binary (.exe / .so)
```

### Commands

| Command | Description |
|---------|-------------|
| `miva init <name> -t <bin\|lib>` | Initialize a new project |
| `miva reinit` | Regenerate `miva.toml` from the template and remove `miva.lock` |
| `miva build [-b <cxx\|llvm\|mvm>]` | Build the project |
| `miva run [-b <cxx\|llvm\|mvm>]` | Build and run (`-b mvm` / `--mvm` runs on the interpreter) |
| `miva clean` | Clean build artifacts |
| `miva sin-build <file>` | Compile a single file |
| `miva sin-run <file>` | Compile and run a single file |
| `miva test <file>` | Run test files |
| `miva get <url>` | Install a dependency |
| `miva dep` | Show the dependency graph starting from `main.miva` |
| `miva <script>` | Run a custom script defined in `[scripts]` |

Options:
- `--release` — Release mode (optimized, `-O2`)
- `--verbose` / `-v` — Verbose output
- `-b <backend>` / `--backend <backend>` — Backend: `cxx` (default), `llvm`, or `mvm`
- `--mvm` — Equivalent to `-b mvm`; builds bytecode and runs it on the MVM interpreter

### Output Structure

```
build/
├── debug/
│   ├── <project_name>      # Debug native executable (cxx/llvm)
│   └── <project_name>.mvm  # Debug bytecode (mvm backend)
└── release/
    ├── <project_name>
    └── <project_name>.mvm

build/debug/cache/
├── src/
│   ├── main.miva.cpp       # Generated C++ source (cxx backend)
│   ├── main.miva.h         # Generated C++ header (exports)
│   ├── main.miva.ll        # Generated LLVM IR (llvm backend)
│   ├── main.miva.mvm       # Generated bytecode (mvm backend)
│   ├── main.miva.o         # Compiled object
│   └── main.miva.sha256    # Source hash for caching
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
