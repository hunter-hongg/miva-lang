# Miva Compiler Error & Warning Codes

## Error Codes (E-series)

| Code | File | Meaning |
|------|------|---------|
| **E0001** | `semantic.rs` | Use of moved value (borrow after move) |
| **E0002** | `semantic.rs` | Cannot move ref parameter / cannot assign to immutable variable |
| **E0004** | `symbol_table.rs` | Duplicate definition — function or struct already defined |
| **E0005** | `semantic.rs` | Module declaration error — not at top of file, duplicate, or multiple module decls |
| **E0007** | `semantic.rs` | Variable not found in scope |
| **E0009** | `semantic.rs` | Cannot call unsafe function from safe context / unknown function (includes ffi) |
| **E0010** | `semantic.rs` | Cannot dereference a ptr in a safe function |
| **E0011** | `semantic.rs` | Choose expression must have an otherwise branch |
| **E0013** | `semantic.rs` | Invalid magical comment directive (only `warning_off`, `warning_err`, `release`, `mangle` allowed) |
| **E0014** | `typecheck.rs` | Type error — catch-all: void value where non-void expected, binop type mismatch, if condition not bool, if/else branch type mismatch, deref non-pointer, etc. |
| **E0016** | `typecheck.rs` | Function call — argument count or argument type mismatch |
| **E0017** | `typecheck.rs` | Return type mismatch (declared return type != actual return type) |
| **E0018** | `typecheck.rs` | Struct literal — unknown struct name, wrong field type, missing field |
| **E0019** | `typecheck.rs` | Field access — unknown field in struct |
| **E0021** | `typecheck.rs` | Invalid cast (e.g. bool -> string) |
| **E0022** | `typecheck.rs` | Assignment type mismatch (variable type != assigned expression type) |
| **E0024** | `typecheck.rs` | Array literal — not all elements have the same type |
| **E0026** | `typecheck.rs` | For loop — range expression must be an array |

## Warning Codes (W-series)

| Code | File | Meaning |
|------|------|---------|
| **W0001** | `warning.rs` | Naming convention violation — function name not snake_case, variable name not snake_case, module name not lowercase |
| **W0002** | `warning.rs` | Deprecated function call (e.g. `prints` -> `prints` macro, `string_concat` -> `std.str.concat`, `ptr_alloc` -> `std.mem.alloc`) |
| **W0003** | `warning.rs` | Invalid intro comment — either format bad (no colon) or annotation type mismatches the following declaration (e.g. `unsafe` before a `safe` function) |

## Quick Reference

```
E0001  use of moved value
E0002  cannot move ref param / assign to immutable
E0004  duplicate function/struct definition
E0005  module decl at wrong position / duplicate / multiple
E0007  variable not found
E0009  unsafe call from safe fn / unknown fn
E0010  deref ptr in safe fn
E0011  choose without otherwise
E0013  invalid magical comment
E0014  type error (generic)
E0016  function arg count/type mismatch
E0017  return type mismatch
E0018  struct literal error
E0019  unknown struct field
E0021  invalid cast
E0022  assignment type mismatch
E0024  array element type mismatch
E0026  for range not array

W0001  naming convention
W0002  deprecated function call
W0003  invalid intro comment / annotation mismatch
```

Total: **18 error codes** + **3 warning codes**.
