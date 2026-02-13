# The MVP Programming Language

## 🚀 Introduction
**MVP** is not just an acronym; it is a triple promise for modern system programming:
1.  **Memory-safe, Verifiable, Predictable**  
    The three pillars of the language. No GC pauses, no undefined behavior.
2.  **Minimal Viable Product**  
    We start from a minimal viable set, rejecting historical baggage and over-engineering.
3.  **Most Valuable Programming Language**  
    A trustworthy modern choice: **Simpler than Rust, Safer than Go, Faster than Python.**
**v0.0.1 is now available!** The compiler is functional and ready for the first wave of pioneers.
---
## ✨ Features
### ✅ Minimalist Ownership System
Achieve memory safety without the complexity of borrow checkers or the latency of Garbage Collection.
*   **`move x`**: Explicit ownership transfer. The original variable immediately becomes invalid.
*   **`clone x`**: Explicit deep copy. No hidden performance costs.
*   **`ref x: T`**: Read-only borrowing (restricted to function parameters, non-escaping).
> **The Rule**: Copy types (primitives) copy automatically. Non-Copy types (buffers, strings) require explicit `move` or `clone`. **No mutable references (`mut ref`) allowed**, eliminating a whole class of data races.
### ✅ Expression-Oriented Syntax
Clean, functional-inspired syntax that values your screen real estate.
```mvp
// Functions are first-class values
foo = (): int => 1
// Structs are defined without noise
Point = struct {
    x: int,
    y: int
}
```
### ✅ Explicit Error Handling
Errors are values, and they cannot be ignored. MVP forces you to handle reality.
```mvp
// Using 'choose' to destructure Result[T, E]
res := choose may_fail() {
    when Ok(value) { value }
    when Err(e) {
        log("Error: ", e)
        return -1
    }
}
```
*   **No `try/catch`**: Exceptions break linear logic.
*   **No `panic` catching**: Panics are fatal crashes, not control flow tools.
### ✅ The `choose` Statement
A cleaner alternative to `switch` and `match`. No fallthrough, no redundant symbols.
```mvp
choose value {
    when 0 { "zero" }
    when 1 { "one" }
    when x if x > 10 { "big number" }
    otherwise { "other" }
}
```
---
## 📦 Syntax Overview
### Variables
Variables are immutable by default. Definition requires initialization.
```mvp
x := 10            // Immutable
mut y := 20        // Mutable
// let z: int      // Error: Variables must be initialized
```
### Functions & Structs
Defined using `=` and `=>`, focusing on expressions.
```mvp
// Function definition
add = (a: int, b: int): int => a + b
// Multi-line function
process = (data: [int]): int => {
    res := compute(data)
    return res
}
```
### Safety Model
MVP introduces a strict safety boundary around FFI and unsafe operations.
1.  **`unsafe`**: For FFI and raw operations. Must be called with `unsafe ffi_func()`.
2.  **`trusted`**: A function that encapsulates `unsafe` code but guarantees memory safety to the caller.
3.  **Safe functions**: Cannot call `unsafe` functions directly. They can only call `trusted` or other safe functions.
> This model is more explicit than Rust's `unsafe` blocks, creating clear safety contracts.
---
## 🚫 What is Explicitly Excluded?
At MVP, what we **don't** add is as important as what we do.
| Feature | Reason |
| :--- | :--- |
| **Garbage Collection (GC)** | Introduces unpredictable latency. Violates "Predictable". |
| **Exceptions (`try/catch`)** | Hidden control flow breaks code readability. |
| **`match` / `switch`** | `switch` has fallthrough issues; `match` is often visually noisy. Use `choose`. |
| **`goto`** | Destructive to structured programming. |
| **Mutable References (`mut ref`)** | Introduces aliasing issues and data races. |
---
## ❤️ The Soul of MVP
*   **No frowning symbols.**
*   **No hidden behaviors.**
*   **No redundant boilerplate.**
*   **No compromise on system power.**
This is not just a design guideline; it is respect for the programmer.
**We trust your judgment, but we don't test your memory.**
---
## 📜 License
MIT License. Use it, break it, improve it.
