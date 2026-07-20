# Higher-Order Functions & Closures in Miva

**Date:** 2026-07-20
**Status:** Approved design

## Goal

Add first-class higher-order functions to Miva:

1. A **function type** `fn(params): ret` in type annotations and signatures.
2. **Closures** written as `(p: t): ret_t => { ... }` — anonymous functions that
   capture variables from the enclosing lexical scope and can be passed around,
   stored, and called.

Supported across **all three codegen backends**: cxx, llvm, and mvm.

## Syntax

### Function type (in annotations / signatures)

```
fn(int, bool): string
fn(fn(int): int, int): int
```

A `fn` type is a comma-separated parameter list in parentheses, a colon, then a
return type. Nested and recursive `fn` types are allowed.

### Lambda / closure literal

```
(p: int): int => { return p + 1; }
(x: int, y: int): int => { return x + y; }
adder = (n: int): fn(int): int => { return (m: int): int => { return m + n; }; };
```

- Parameters are `(name: type, ...)` — explicit types, required.
- Return type after the colon — **explicit, required**.
- Body is a `{ ... }` block (statements + trailing result expression, same shape
  as a `DFunc` body).
- A lambda may reference variables declared in enclosing scopes. Those become
  **captures**.

## Architecture

### 1. Frontend AST (`miva-frontend-rs/src/ast.rs`)

- Add `Typ::TFunc { params: Vec<Typ>, returns: Box<Typ> }` to the `Typ` enum.
  Serialized as `{"kind":"func","params":[...],"returns":...}`.
- Add an `Expr` variant `ELambda`:
  ```rust
  #[serde(rename = "lambda")]
  ELambda {
      loc: Loc,
      params: Vec<Param>,
      #[serde(rename = "type")]
      returns: Typ,
      body: Box<Expr>,
  }
  ```
  Capture info is **not** stored in the frontend AST — it is computed during
  typechecking (the backend AST keeps this field too, filled in later).

### 2. Backend AST (`miva/src/ast.rs`)

- Same `Typ::TFunc` variant (must stay byte-compatible with the frontend JSON).
- Same `ELambda` variant, with an extra `captures: Vec<(String, Typ)>` field
  added by the typechecker:
  ```rust
  #[serde(rename = "lambda")]
  ELambda {
      loc: Loc,
      params: Vec<Param>,
      #[serde(rename = "type")]
      returns: Typ,
      captures: Vec<(String, Typ)>,
      body: Box<Expr>,
  }
  ```
  The frontend emits `captures` empty/absent; the typechecker populates it.

### 3. Frontend parser (`miva-frontend-rs/src/parser.rs`)

- `parse_typ`: recognize `fn` keyword → parse `(param_types)` then `:` ret.
  Param types are `parse_typ` separated by commas. Recursive for nested fn types.
- `parse_expr` / primary expression: when the next token is `(` followed by
  `ident :` (a typed parameter), parse a lambda:
  1. `(` param list (`name : type`, comma-separated) `)`
  2. `:` return type
  3. `=>` (reuse/expect existing `DArrow` token; if not present, add it)
  4. `{` block `}`
  Build `Expr::ELambda`. A parenthesized expression that is `(expr)` with no
  typed params stays a grouping expr (existing behavior). Disambiguate by
  peeking: `(` `ident` `:` ⇒ lambda; otherwise grouping.

### 4. Typechecker (`miva/src/typecheck.rs`)

- `infer_type` gains an `Expr::ELambda` arm:
  - Create a child `TypeEnv` (so captures can be detected).
  - Declare params in the child env with their annotated types.
  - Infer the body with `func_return = Some(&returns)`.
  - Check the inferred body type unifies with `returns`.
  - Compute `captures`: every variable the lambda reads that is **not** a param
    and **not** declared in the lambda's own env ⇒ a capture from the enclosing
    env. Collect `(name, type)` pairs in declaration order.
  - Return type `Typ::TFunc { params, returns }`.
- `loc_of` gains the `ELambda` arm.
- `normalize_typ` / `resolve_type` / `infer_type_from_arg` gain `TFunc` handling
  (recurse into params + returns). Generic fn types are rare but must not panic.
- Variable lookup for captures: extend `TypeEnv` so that when a name resolves to
  an enclosing scope (not the current lambda), it is recorded as captured.
- `ECall` typechecking: when the callee expression has type `TFunc`, validate
  argument types against `params` and the result type against `returns`.
- `SLetTyped` / `SAssign` accept a `TFunc` value on the RHS matching the declared
  `TFunc` type.
- `DFunc` params/returns of `TFunc` type are allowed (higher-order functions).

### 5. Codegen — captured environment representation

Each closure is lowered to **two parts** at its definition site:

- A **closure environment struct** holding the captured variables by value,
  generated with a unique name (e.g. `__closure_<id>`). For recursive / nested
  lambdas the env struct and the thunk function are both emitted.
- A **thunk function** with signature `(env_ptr, arg0, arg1, ...) -> ret`:
  it first loads captured vars out of the env, then runs the body.

At a **call site** `f(args)`:
1. If `f` is a closure value (a `TFunc`-typed variable / result), build the env
   struct from the captured vars in scope, take its address, and call the thunk
   with `(env_ptr, args...)`.
2. If `f` names a top-level `DFunc` of matching `TFunc` type, call it directly
   (no env).

The `ELambda` itself, when used as a value (e.g. returned / assigned), desugars
to "construct env + reference to thunk" — the backend represents a closure value
as a struct `{ env_ptr, thunk_ptr }` (or, where simpler, inlines the construction
at each use site).

#### 5a. cxx backend (`miva/src/codegen/cxx.rs`)

- `cxx_type(Typ::TFunc {..})` → a `std::function<ret(params...)>`-like type, or a
  hand-rolled `struct { void* env; ret(*fn)(void*,args); }`. Use the struct form
  for uniform calling.
- `cxx_expr(ELambda)`: emit a lambda-local "make closure" — declare the env
  struct (captures copied in by `[&]`-captured reference at construction), define
  the thunk as a static `[&]`-free function taking `env*`, and return the
  closure struct value.
- `cxx_call` / `gen_call`: if the callee is a closure-typed expression, unpack
  `env` + `fn`, build env, and emit `fn(env, args...)`.

#### 5b. llvm backend (`miva/src/codegen/llvm.rs`)

- Add an LLVM struct type for each distinct `TFunc` signature (or one generic
  `{ i8*, ptr }` fat shape).
- `ELambda`: emit an internal function `@__closure_<id>(i8* env, ...)` and build
  a constant/stack closure value `{ env, @__closure_<id> }`.
- `gen_call`: when callee type is `TFunc`, load env + fn pointer and `call`.

#### 5c. mvm backend (`miva/src/codegen/mvm.rs` + `miva-vm`)

- Closures represented as a VM value: a struct `{ func_index: u32, env: ... }`.
- Add an opcode `MakeClosure` (func_idx, n_captures) that captures `n_captures`
  values from the stack into an env block, and `CallClosure` (pops closure +
  args, sets up env as an extra local frame, jumps to func).
- `ELambda` compiles to: a new top-level `MvmFunction` (the thunk) + a
  `MakeClosure` at the use site that closes over captured locals.
- Env is passed as an implicit first local in the thunk.

## Data Flow

```
source  →  frontend parser  →  frontend AST (ELambda, TFunc)
  →  JSON  →  backend AST (same, captures=[])
  →  typechecker  →  fills ELambda.captures, validates TFunc types
  →  codegen (cxx | llvm | mvm)  →  native / bytecode
```

## Error Handling

- Lambda param type missing → parse error.
- Lambda return type missing → parse error.
- Return type mismatch with body → `E00xx` type error.
- Calling a non-`TFunc` value, or arg/return arity/type mismatch → type error.
- Capture of a variable that does not exist in any enclosing scope → type error.

## Testing

- `miva-frontend-rs`: unit test parsing `(p: int): int => {...}` and `fn(int): int`.
- `miva`: typecheck tests for valid + invalid closures (mismatch, bad capture).
- End-to-end examples under `examples/` covering:
  - passing a closure to a higher-order `DFunc`,
  - returning a closure (adder factory),
  - nested closures capturing multiple vars,
  - storing a closure in a `let` and calling it later.
- Run the project's existing test harness (`rtk ... test`) for cxx, llvm, mvm.

## Out of Scope (YAGNI)

- Closures that mutate captured variables back into the enclosing scope
  (captures are by value / read-only for v1).
- Generic higher-kinded `fn` types with type parameters.
- Async closures (`is_async` lambdas).
- Closure equality / hashing.
