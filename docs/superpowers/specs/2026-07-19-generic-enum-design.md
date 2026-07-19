# Generic Enum Support — `Name[T] = enum { A(T), B }`

Date: 2026-07-19

## Goal

Make algebraic-data-type enums generic, mirroring generic structs. Syntax:

```
Name[T] = enum {
    Empty,
    Value(T),
    Pair(T, string),
}
```

Construct with `Name[T].Value(x)` (explicit type args) or `Name.Value(x)` (inferred
from payload argument types). Match with the existing `choose`/`when` and access
payload positionally via `x.0`, `x.1`.

Work end-to-end: lexer → parser → AST → typecheck → three backends
(C++ `cxx.rs`, LLVM `llvm.rs`, MVM `miva-vm`).

## 1. Syntax

- Top-level definition: `Ident [TypeParams] = enum { Variant, ... }` (parser already
  supports `type_params` for `DEnum`).
- Each variant: `VariantName` or `VariantName ( Type, ... )`.
- Generic enum params allowed: `Name[T] = enum { ... }`.
- Construction:
  - Explicit: `Name[T].Variant(args)` → `ECall { name: "Name.Variant", type_args: [T], args }`.
  - Inferred: `Name.Variant(args)` → `type_args: vec![]`, inferred from args.
  - Desugared method-call form after macro expansion: `Variant(EnumName, args)` and
    `Variant(EnumName[T], args)` — thread `type_args` through `enum_pattern_or_call`.
- Patterns (in `when`): `Name.Variant(x, y)` — no type args needed; the scrutinee's
  type supplies them.

## 2. AST

`miva-frontend-rs/src/ast.rs` and `miva/src/ast.rs` already have:
- `DEnum { loc, name, variants, type_params: Vec<String> }`.
- `ECall { name, type_args, args }` — already carries `type_args`; enum constructors
  must populate it.

No new AST variants. Enum type is represented as `Typ::TStruct { name, type_args, .. }`
(reusing existing type machinery), as with non-generic enums.

## 3. Parser / frontend

In `miva-frontend-rs/src/parser.rs`:
- `parse_enum_body` already threads `type_params` through `Def::DEnum`. ✅
- Enum constructor calls: the dotted `Name.Variant` and desugared `Variant(EnumName, ...)`
  paths currently discard type args. Add optional `[TypeArgs]` parsing (mirror the
  struct/function call handling at parser.rs:938-964) and carry `type_args` into the
  produced `ECall`. In `enum_pattern_or_call`, accept the new `type_args` parameter for
  both the `EEnumPattern` (unused) and the `ECall` constructor paths.

## 4. Typecheck (`miva/src/typecheck.rs`)

- `build_enum_map` → also build `enum_type_params: HashMap<String, Vec<String>>`
  (mirror `build_struct_map`).
- `infer_type` `ECall` enum constructor paths (dotted + desugared):
  - Build `type_subst` from `enum_type_params` to `normalize_typ(type_args, ...)` with
    inference fallback from payload arguments (mirror `EStructLit`).
  - Normalize each variant payload type through `type_subst`, check arg types against it.
  - Return `Typ::TStruct { name, fields: vec![], type_args: normalized_type_args }`.
- `EEnumPattern` and `EFieldAccess` discriminant: return the enum `TStruct` carrying the
  scrutinee's resolved `type_args` so `choose`'s `when`-type vs scrutinee-type comparison
  matches under `types_equal` (which already compares `TStruct` by name + `type_args`).
  Propagate the scrutinee var's `type_args` into the pattern type during `EChoose`
  handling (the `choose` `var` type already carries them).
- `types_equal` already recurses on `type_args`, so generic enum identity works once
  `type_args` are populated.

## 5. Codegen

### cxx.rs
- `cxx_enum_def(name, type_params, variants, ...)` already receives `type_params` but
  ignores them. Emit a `template<typename T, ...>` header for generic enums and reference
  the params in payload field types. Constructors become `Name<T> Name_T(...)` /
  `Name<T> Name_T()` (templated return type + name).
- `cxx_call` enum constructor branch: append `<...>` type args when `type_args` is
  non-empty (mirror `cxx_struct_lit`).

### llvm.rs
- Enumerate types are monomorphic. Add a pre-pass that collects concrete
  `(enum_name, type_args)` instantiations used in the program, and emit one tagged struct
  type per instantiation (e.g. `%Name_T_int`). Constructor / tag-store / `icmp` / payload
  GEP codegen keyed by the concrete instantiation name. `choose`/`when` compares the tag
  field (type-independent).

### miva-vm
- Runtime is dynamically typed; generic args are erased at runtime. `EnumNew`/`EnumGet`/
  `choose`/`when` operate on `(tag, payload...)` regardless of `T`. Frontend accepts the
  `[T]` syntax; codegen ignores `type_args` for enums. No new opcodes.

## 6. Tests

- `examples/generic-enum.mv`: `Box[T] = enum { Empty, Value(T) }` — construct with
  explicit and inferred type args, `choose`/`when` match, `.0` payload access, build &
  run on all three backends.
- Parser test: generic enum definition round-trips to `Def::DEnum` with `type_params`.
- Typecheck test: generic enum constructor arg-type checking + identity with inferred
  `type_args`.

## Scope / Non-goals
- No exhaustive-match exhaustiveness checking (relies on `otherwise`).
- No pattern nesting.
- Named payload fields deferred (positional only).
- miva-vm treats generics as type-erased (no runtime distinction between `Box[int]` and
  `Box[string]`).
