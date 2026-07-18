# ADT Enum Design — `xxx = enum { A(int), B(int, string) }`

Date: 2026-07-18

## Goal

Add algebraic data type (ADT) enums to Miva. Syntax:

```
Name = enum {
    A(int),
    B(int, string),
    C,            // unit variant, no payload
}
```

Construct values with `Name.A(args)` and match with the existing `choose`/`when`
by tag, accessing payload positionally via `x.0`, `x.1`.

Implemented end-to-end: lexer → parser → AST → symbol table → typecheck → three
backends (C++ `cxx.rs`, LLVM `llvm.rs`, MVM `miva-vm`).

## 1. Syntax

- New keyword `enum`.
- Top-level definition: `Ident [TypeParams] = enum { Variant, Variant, ... }`.
- Each variant: `VariantName` or `VariantName ( Type, Type, ... )`.
  - Payload fields are **positional** and **typed**.
  - A variant with no payload is a unit variant.
- Generic enum params allowed for symmetry with structs: `Name[T] = enum { ... }`.

## 2. AST changes

`miva-frontend-rs/src/ast.rs`:

```rust
pub struct EnumVariant {
    pub name: String,
    pub payload: Vec<Typ>,
}

// In Def enum:
#[serde(rename = "enum")]
DEnum {
    loc: Loc,
    name: String,
    variants: Vec<EnumVariant>,
    #[serde(default)]
    type_params: Vec<String>,
}
```

The enum type itself is represented as `Typ::TStruct { name, .. }` (name-only),
reusing existing type machinery for identity/equality so we do not add a new
`Typ` variant. Variant data lives in the symbol table.

## 3. Construction and matching (usage)

- **Construct**: `Name.A(1, "x")` parses as field-access `Name.A` applied to
  args → emitted as `ECall { name: "Name.A", args }`. Codegen emits a
  constructor returning the tagged-union value.
- **Match**: reuse `choose`/`when`:
  ```
  choose (x) {
      when (Name.A) { let n = x.0; ... }
      when (Name.B) { let n = x.0; let s = x.1; ... }
      otherwise { ... }
  }
  ```
  `when (Name.A)` parses as `EFieldAccess { EVar("Name"), "A" }`, treated as a
  discriminant literal. Equality compares the tag field.
- **Payload access**: `x.0`, `x.1` → `EFieldAccess { expr, field: "0" }` →
  codegen emits `.__payload.field0`.

## 4. Symbol table

`miva/src/symbol_table.rs`:

```rust
pub struct EnumEntry {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub type_params: Vec<String>,
}
```

- Add `enums: Vec<EnumEntry>` + `enum_index: HashMap<String, usize>`.
- Register in `build_with_errors` (`Def::DEnum`).
- Add `lookup_enum`.

## 5. Typecheck

`miva/src/typecheck.rs`:

- Add `build_enum_map` mirroring `build_struct_map`.
- `infer_type`:
  - `Name.A(args)` call: resolve enum, validate arg count/types against variant
    payload, return `Typ::TStruct { name }`.
  - `when (Name.A)` variant access: type the discriminant as the enum type.
- `types_equal` already compares `TStruct` by name — enum type identity works.

## 6. Codegen — manual tagged union

For enum `Name` with variants V0..Vn, C++ representation:

```cpp
struct Name {
    mvp_builtin_int __tag;
    struct {
        // union of all payload fields, sized to the largest variant
        mvp_builtin_int field0;
        mvp_builtin_string field1;
        // ...
    } __payload;
};

inline Name Name_A(mvp_builtin_int a0) {
    Name v;
    v.__tag = 0;
    v.__payload.field0 = a0;
    return v;
}
```

- `when (Name.A)` → `x.__tag == 0`.
- `x.0` → `x.__payload.field0`.

Per backend:

- **cxx.rs**:
  - `cxx_def` handles `Def::DEnum` → emit struct + per-variant constructor funcs.
  - `cxx_expr` handles `ECall` with name `"Name.A"` (constructor) and
    `EFieldAccess` with numeric field → payload access.
- **llvm.rs**: emit tagged struct type, constructor as struct build + tag
  store, match as tag `icmp`, payload access as GEP into the payload struct.
- **miva-vm**: register enum as a runtime tagged value; constructor pushes
  `(tag, payload...)`; `choose`/`when` reads the tag; `.0` reads payload slot.

## 7. Tests

- Lexer: `enum` keyword token.
- Parser: `Color = enum { Red, Green(int) }` round-trips to `Def::DEnum`.
- Codegen (`cxx.rs`): construct + `choose`/`when` emits correct C++ with
  `__tag`/`__payload`.
- Example: `examples/enum.mv` exercising construct + match, builds & runs on
  all three backends.

## Scope / Non-goals

- No exhaustive-match exhaustiveness checking in this pass (rely on
  `otherwise`).
- No pattern nesting (no `when (Name.A(Name2.X))`).
- Named payload fields deferred (positional only, per decision).
