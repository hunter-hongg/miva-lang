# Miva Programming Language Compiler

The Miva compiler - a full-stack compiler that parses Miva source code and compiles it to native binaries via C++.

## Build

```bash
cargo build --release
```

## Usage

### Init project

```bash
cargo run -- --init
```

### Build

```bash
cargo run -- --build --release
```

### Run

```bash
cargo run -- --run --release
```

### Test

```bash
cargo run -- --test test_hello.miva
```

### Clean

```bash
cargo run -- --clean
```

## Language

The Miva language is compiled to C++ code, which is then compiled to native machine code using g++.

### Type system

- `int`
- `float32`
- `float64`
- `bool`
- `string`
- `char`
- `array<T>`
- `ptr<T>`
- `box<T>`
- `null`
- `ptrany`

### Safety levels

- `safe` - safe by default
- `unsafe` - requires explicit declaration
- `trusted` - special function that bypasses safety checks

### Example

```miva
// main.miva
export func main() -> int {
    let x: int = 10
    let y: int = 20
    return x + y
}
```

Build and run:

```bash
cargo run -- --build --release
cargo run -- --run --release
```

## Architecture

- Miva source → AST → Symbol Table → Semantic Analysis → Type Checking →
  Macro Expansion → C++ Header → C++ Source → g++ → Object Files → Link → Native Binary
