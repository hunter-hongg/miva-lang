# Higher-Order Functions & Closures Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add first-class higher-order functions to Miva: a `fn(params): ret` function type, and closure literals `(p: t): ret_t => { ... }` that capture enclosing variables, supported across the cxx, llvm, and mvm backends.

**Architecture:** A closure is represented uniformly as an environment struct (captured vars by value) plus a thunk function `(env*, args...) -> ret`. The frontend parser produces `ELambda`/`TFunc` AST nodes; the typechecker computes the capture set and validates types; each backend lowers closures to its native function-pointer + env representation. The cxx backend leverages its existing `([&]() { ... })()` lambda machinery; llvm and mvm get explicit closure support.

**Tech Stack:** Rust (frontend `miva-frontend-rs`, compiler `miva`), C++ (cxx backend), LLVM IR (llvm backend), custom bytecode VM (mvm backend + `miva-vm`).

---

## File Structure

- `miva-frontend-rs/src/ast.rs` — add `Typ::TFunc` and `Expr::ELambda` (captures empty in frontend).
- `miva-frontend-rs/src/lexer.rs` — already has `Token::DArrow` (`=>`); no change needed (verify).
- `miva-frontend-rs/src/parser.rs` — `parse_typ` handles `fn(...): ...`; `parse_expr` handles `(params): ret => {...}`.
- `miva/src/ast.rs` — add matching `Typ::TFunc` and `Expr::ELambda { captures: Vec<(String, Typ)> }`.
- `miva/src/typecheck.rs` — `infer_type`/`require_type`/`normalize_typ`/`resolve_type`/`infer_type_from_arg`/`loc_of` handle `TFunc` + `ELambda`; capture analysis.
- `miva/src/codegen/cxx.rs` — `cxx_type` for `TFunc`; `cxx_expr` for `ELambda`; call-site closure dispatch.
- `miva/src/codegen/llvm.rs` — `TFunc` type; `ELambda` emission; `CallClosure`.
- `miva/src/codegen/mvm.rs` — closure value model; `ELambda` compile; new opcodes.
- `miva-vm/src/opcode.rs` — add `MakeClosure`, `CallClosure`, `CloseEnv` opcodes + metadata.
- `miva-vm/src/value.rs` — add `Value::Closure(usize /*func idx*/, Arc<Vec<Value>> /*env*/)`.
- `miva-vm/src/vm.rs` — execute new opcodes.
- `miva/src/semantic.rs`, `miva/src/warning.rs` — extend `check_expr` to tolerate `ELambda` and `TFunc` (no errors).
- Tests: `miva-frontend-rs/src/parser.rs` (unit), `miva/test/` typecheck tests, `examples/` e2e programs.

---

## Task 1: Frontend AST — `Typ::TFunc` and `Expr::ELambda`

**Files:**
- Modify: `miva-frontend-rs/src/ast.rs`

- [ ] **Step 1: Add `TFunc` to the `Typ` enum** (after `TGenericParam`, before closing brace at line 69)

```rust
    #[serde(rename = "func")]
    TFunc {
        #[serde(rename = "params")]
        params: Vec<Typ>,
        #[serde(rename = "returns")]
        returns: Box<Typ>,
    },
```

- [ ] **Step 2: Add `ELambda` to the `Expr` enum** (after `EEnumPattern`, before closing brace at line 331)

```rust
    #[serde(rename = "lambda")]
    ELambda {
        loc: Loc,
        params: Vec<Param>,
        #[serde(rename = "type")]
        ret: Typ,
        body: Box<Expr>,
    },
```

- [ ] **Step 3: Verify it compiles**

Run: `cd /home/hunter/projects/miva-lang/miva-frontend-rs && cargo build`
Expected: BUILD success (no usages yet, exhaustive matches will fail later in dependents — that's fine).

- [ ] **Step 4: Commit**

```bash
git add miva-frontend-rs/src/ast.rs
git commit -m "feat(frontend-ast): add TFunc type and ELambda expr variants"
```

---

## Task 2: Backend AST — matching `Typ::TFunc` and `Expr::ELambda` with captures

**Files:**
- Modify: `miva/src/ast.rs`

- [ ] **Step 1: Add `TFunc` to `Typ`** (after `TGenericParam`, before line 97)

```rust
    #[serde(rename = "func")]
    TFunc {
        #[serde(rename = "params")]
        params: Vec<Typ>,
        #[serde(rename = "returns")]
        returns: Box<Typ>,
    },
```

- [ ] **Step 2: Add `ELambda` to `Expr`** (after `EEnumPattern`, before line 318). Captures filled by typechecker.

```rust
    #[serde(rename = "lambda")]
    ELambda {
        loc: Loc,
        params: Vec<Param>,
        #[serde(rename = "type")]
        ret: Typ,
        #[serde(default)]
        captures: Vec<(String, Typ)>,
        body: Box<Expr>,
    },
```

- [ ] **Step 3: Verify it compiles**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: BUILD success (other files will have non-exhaustive match errors later; acceptable now).

- [ ] **Step 4: Commit**

```bash
git add miva/src/ast.rs
git commit -m "feat(ast): add TFunc and ELambda with captures to backend AST"
```

---

## Task 3: Frontend parser — `fn` type and lambda literals

**Files:**
- Modify: `miva-frontend-rs/src/parser.rs`
- Test: `miva-frontend-rs/src/parser.rs` (add unit tests in `mod tests`)

- [ ] **Step 1: Write failing parser tests** (add to the `#[cfg(test)] mod tests` block)

```rust
    #[test]
    fn test_parse_func_type() {
        let ast = crate::ast::Typ::TInt;
        let _ = ast;
        let defs = parse("f: fn(int, int): int => { return 0; }", "t.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn test_parse_lambda() {
        let defs = parse("main = () => { let g = (x: int): int => { return x + 1; }; return 0; }", "t.miva").unwrap();
        assert_eq!(defs.len(), 1);
    }
```

- [ ] **Step 2: Run tests, confirm fail**

Run: `cd /home/hunter/projects/miva-lang/miva-frontend-rs && cargo test test_parse_func_type test_parse_lambda`
Expected: FAIL (parser does not yet handle `fn` type or lambda head).

- [ ] **Step 3: Extend `parse_typ` to handle `fn`** — locate `fn parse_typ` (~line 648). At the top of its match on the next token, add a `Token::Fn` arm. First find the `Fn` keyword token: it must be added to the lexer keyword map. Check `lexer.rs` for the keyword table (around line 640) and add `"fn" => Token::Fn`. Then add the `Fn` variant to `Token` enum (~line 5) and the `expect_keyword` set if needed. In `parse_typ`:

```rust
    fn parse_typ(&mut self) -> Result<Typ, String> {
        match self.peek_token()? {
            Some(&Token::Fn) => {
                self.advance()?; // consume "fn"
                self.expect(&Token::LParen)?;
                let mut params = Vec::new();
                if self.peek_token()? != Some(&Token::RParen) {
                    loop {
                        params.push(self.parse_typ()?);
                        if self.peek_token()? == Some(&Token::Comma) {
                            self.advance()?;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(&Token::RParen)?;
                self.expect(&Token::Colon)?;
                let returns = Box::new(self.parse_typ()?);
                Ok(Typ::TFunc { params, returns })
            }
            // ... existing arms unchanged ...
        }
    }
```

- [ ] **Step 4: Parse lambda in `parse_expr` primary** — find the primary-expression parser (the function that handles `(` grouping / literals). Add a branch: when next tokens are `(`, an identifier, then `:`, parse a lambda. Implement a helper:

```rust
    fn parse_lambda(&mut self) -> Result<Expr, String> {
        let start = self.peek_token()?.cloned().map(|(p, _, _)| p).unwrap_or(0);
        // peek: '(' ident ':'
        let is_lambda = matches!(
            (self.peek_token()?, self.peek_token_nth(1)?, self.peek_token_nth(2)?),
            (Some(&Token::LParen), Some(&Token::Ident(_)), Some(&Token::Colon))
        );
        if !is_lambda {
            return Err("expected lambda".to_string());
        }
        self.expect(&Token::LParen)?;
        let mut params = Vec::new();
        if self.peek_token()? != Some(&Token::RParen) {
            loop {
                let (name, _) = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let typ = self.parse_typ()?;
                params.push(Param::POwn { name, typ });
                if self.peek_token()? == Some(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
        }
        self.expect(&Token::RParen)?;
        self.expect(&Token::Colon)?;
        let ret = self.parse_typ()?;
        self.expect(&Token::DArrow)?;
        let body = self.parse_block_expr()?; // the existing block parser used by `=> { ... }`
        Ok(Expr::ELambda {
            loc: self.loc(start),
            params,
            ret,
            body: Box::new(body),
        })
    }
```

Wire `parse_lambda` into the primary-expression dispatch: before the existing `(` grouping branch, check `is_lambda_head` and call `parse_lambda`. Also allow `fn`-typed params in `parse_typ` already done. Add `peek_token_nth(n)` helper that peeks `n` tokens ahead using the lexer's clone/backtrack (mirror existing `peek_token`).

- [ ] **Step 5: Run tests, confirm pass**

Run: `cd /home/hunter/projects/miva-lang/miva-frontend-rs && cargo test test_parse_func_type test_parse_lambda`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add miva-frontend-rs/src/parser.rs miva-frontend-rs/src/lexer.rs miva-frontend-rs/src/ast.rs
git commit -m "feat(parser): parse fn(...) type and (params): ret => {...} lambdas"
```

---

## Task 4: Typechecker — `TFunc` type handling

**Files:**
- Modify: `miva/src/typecheck.rs`

- [ ] **Step 1: Extend `normalize_typ`** (lines 8-39) with `TFunc` arm:

```rust
        Typ::TFunc { params, returns } => Typ::TFunc {
            params: params.iter().map(|p| normalize_typ(p, type_params)).collect(),
            returns: Box::new(normalize_typ(returns, type_params)),
        },
```

- [ ] **Step 2: Extend `resolve_type`** (lines 66-92) with `TFunc` arm:

```rust
        Typ::TFunc { params, returns } => Typ::TFunc {
            params: params.iter().map(|p| resolve_type(p, subst)).collect(),
            returns: Box::new(resolve_type(returns, subst)),
        },
```

- [ ] **Step 3: Extend `infer_type_from_arg`** (lines 95-119) with `TFunc` arm:

```rust
        (Typ::TFunc { params: pp, returns: pr }, Typ::TFunc { params: ap, returns: ar })
            if pp.len() == ap.len() =>
        {
            for (pt, at) in pp.iter().zip(ap.iter()) {
                infer_type_from_arg(pt, at, subst);
            }
            infer_type_from_arg(pr, ar, subst);
        }
```

- [ ] **Step 4: Verify build (will have match errors elsewhere, fix those in later tasks)**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: typecheck.rs compiles; remaining errors are in codegen/semantic for unhandled `ELambda` (resolved in Tasks 5-9).

- [ ] **Step 5: Commit**

```bash
git add miva/src/typecheck.rs
git commit -m "feat(typecheck): handle TFunc in normalization and substitution"
```

---

## Task 5: Typechecker — lambda type inference & capture analysis

**Files:**
- Modify: `miva/src/typecheck.rs`

- [ ] **Step 1: Add a capture-tracking `TypeEnv`** — inspect the existing `TypeEnv` struct (search `struct TypeEnv`). Add a field `captured: Vec<(String, Typ)>` and a method `lookup_capture(name) -> Option<Typ>` that searches enclosing scopes. When `infer_type` resolves an `EVar`/`EMove`/`EClone` to a variable NOT in the current lambda scope but present in an outer scope, append it to `captured` (dedup by name).

- [ ] **Step 2: Add `ELambda` arm in `infer_type`** (the big `match e` at line 398). Insert after `Expr::EEnumPattern` arm or near other complex exprs:

```rust
        Expr::ELambda {
            loc,
            params,
            ret,
            body,
            ..
        } => {
            // Child env: params are declared locally; everything else is captured.
            let mut child_env = env.child_scope();
            for p in params {
                let (name, typ) = match p {
                    Param::PRef { name, typ } => (name.clone(), typ.clone()),
                    Param::POwn { name, typ } => (name.clone(), typ.clone()),
                };
                child_env.declare(name, typ);
            }
            let (body_typ, body_errs) = infer_type(
                &mut child_env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                &Some(ret.clone()),
                body,
            );
            let mut errs = body_errs;
            // Validate body return type matches declared ret.
            if !unify(&body_typ, ret) {
                errs.push(Error::new(
                    "E0020",
                    &loc_of(body),
                    &format!("lambda body type {:?} does not match declared return {:?}", body_typ, ret),
                ));
            }
            let captures: Vec<(String, Typ)> = child_env.take_captures();
            (Typ::TFunc {
                params: params.iter().map(|p| param_typ(p)).collect(),
                returns: Box::new(ret.clone()),
            }, errs)
        }
```

Add helpers `unify(a, b)` (deep structural equality using existing `Typ` `PartialEq`) and `param_typ(p)`. Add a `take_captures` method to `TypeEnv`.

- [ ] **Step 3: Add `ELambda` arm to `loc_of`** (lines 356-386):

```rust
        | Expr::ELambda { loc, .. } => loc.clone(),
```

- [ ] **Step 4: Handle `TFunc` in `ECall` typechecking** — locate the `Expr::ECall` arm in `infer_type` (~line 910). When the callee resolves to a `Typ::TFunc { params, returns }`, validate each argument against `params` and the overall result as `returns`. (Reuse existing argument-checking logic already present for named functions.)

- [ ] **Step 5: Verify build**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: typecheck.rs compiles with lambda handling.

- [ ] **Step 6: Commit**

```bash
git add miva/src/typecheck.rs
git commit -m "feat(typecheck): infer TFunc for lambdas and compute capture set"
```

---

## Task 6: semantic.rs & warning.rs — tolerate `ELambda` / `TFunc`

**Files:**
- Modify: `miva/src/semantic.rs`, `miva/src/warning.rs`

- [ ] **Step 1: In `semantic.rs` `check_expr`** (line 53) add an `Expr::ELambda { .. } => vec![]` arm so lambdas are not flagged as unhandled.

- [ ] **Step 2: In `warning.rs` `check_expr`** (line 148) add `Expr::ELambda { .. } => {}` arm.

- [ ] **Step 3: Build**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: BUILD success (codegen match errors remain — fixed in Tasks 7-9).

- [ ] **Step 4: Commit**

```bash
git add miva/src/semantic.rs miva/src/warning.rs
git commit -m "feat(semantic): tolerate ELambda in semantic and warning passes"
```

---

## Task 7: cxx backend — `TFunc` type and `ELambda` lowering

**Files:**
- Modify: `miva/src/codegen/cxx.rs`

- [ ] **Step 1: `cxx_type` for `TFunc`** — locate `fn cxx_type` (~line 66). Add arm returning a C++ function-object type. Use a struct holding env + thunk:

```rust
    Typ::TFunc { params, returns } => {
        let ps: Vec<String> = params.iter().map(|p| cxx_type(p)).collect();
        let r = cxx_type(returns);
        format!("miva_closure<{}>", ps.into_iter().chain(std::iter::once(r)).collect::<Vec<_>>().join(","))
    }
```

Add a C++ template `miva_closure` declaration to the cxx prelude/header (search for where struct/type prelude is emitted, e.g. `cxx_program` top). Define:

```cpp
template<typename R, typename... A>
struct miva_closure {
    void* env;
    R (*fn)(void*, A...);
    R operator()(A... a) const { return fn(env, a...); }
};
```

- [ ] **Step 2: `cxx_expr` for `ELambda`** — add arm (in `match expr` at line 343). Generate a unique closure id (`__closure_<counter>`), emit an env struct capturing `captures` by value, a static thunk function `(void* env, args...) -> ret`, and return a `miva_closure` value. Use the existing lambda/`[&]` capture mechanism to read captured vars into the env at construction and out of `env` in the thunk:

```rust
        Expr::ELambda { params, ret, captures, body, .. } => {
            let cid = cxx_closure_id();
            let env_name = format!("__env_{}", cid);
            let thr_name = format!("__thunk_{}", cid);
            let cap_decls: Vec<String> = captures.iter().map(|(n, t)| format!("{} {};", cxx_type(t), n)).collect();
            let cap_init: Vec<String> = captures.iter().map(|(n, _)| format!("{} = {};", n, mangle_cpp_kw(n))).collect();
            let thr_params: Vec<String> = std::iter::once("void* __env_ptr".to_string())
                .chain(params.iter().map(|p| format!("{} {}", cxx_param_type(p), cxx_param_name(p)))).collect();
            let thr_body = cxx_expr(body, depth + 1, Some(&cxx_type(ret)));
            let env_loads: Vec<String> = captures.iter().map(|(n, t)| {
                format!("{} {} = static_cast<{}*>(__env_ptr)->{};", cxx_type(t), n, cxx_type(t), n)
            }).collect();
            format!(
                "([&]() {{ struct __Env_{cid} {{ {caps} }}; static auto {thr} = +[]({tps}) -> {rt} {{ {loads} return {body}; }}; __Env_{cid} {env}; {inits} return miva_closure<...>{{ &{env}, {thr} }}; }})()",
                cid = cid, caps = cap_decls.join(" "), thr = thr_name,
                tps = thr_params.join(", "), rt = cxx_type(ret),
                loads = env_loads.join(" "), body = thr_body, env = env_name,
                inits = cap_init.join(" ")
            )
        }
```

Keep the `miva_closure<...>` template-arg list consistent with `cxx_type`. (This is a schematic; match the existing cxx emission helpers exactly.)

- [ ] **Step 3: Closure call dispatch** — in `cxx_call` / the `ECall` path, when the callee `name` is a variable whose type is `TFunc`, emit `callee(args...)` where `callee` is a `miva_closure` (operator() handles it). No change needed if the closure value is used directly as an `EVar`.

- [ ] **Step 4: Build & smoke test**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: BUILD success. Then compile a tiny example (Task 12) to confirm C++ output.

- [ ] **Step 5: Commit**

```bash
git add miva/src/codegen/cxx.rs
git commit -m "feat(cxx): lower TFunc and ELambda closures to miva_closure + thunks"
```

---

## Task 8: llvm backend — `TFunc` and `ELambda`

**Files:**
- Modify: `miva/src/codegen/llvm.rs`

- [ ] **Step 1: Map `TFunc` to an LLVM struct type** — in the LLVM type builder, when encountering `Typ::TFunc`, emit a struct `{ i8*, ptr }` (env ptr + fn ptr). Add arm to `llvm_type(typ)`.

- [ ] **Step 2: `ELambda` emission** — add an `Expr::ELambda` arm in the LLVM expression compiler (mirror `gen_call` area ~line 1052). Generate:
  1. An internal function `@__closure_<id>(i8* %env, <arg types>...) -> <ret type>` that bitcasts `%env` to the env struct, loads captures, then compiles `body`.
  2. At the use site, allocate the env struct on the stack, store captured values, bitcast to `i8*`, and build the closure struct `{ i8* env, ptr @__closure_<id> }`.

- [ ] **Step 3: Closure call** — when compiling `ECall` whose callee type is `TFunc`, extract env + fn from the closure struct, then `call <ret> %fn(i8* %env, args...)`.

- [ ] **Step 4: Build**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: BUILD success.

- [ ] **Step 5: Commit**

```bash
git add miva/src/codegen/llvm.rs
git commit -m "feat(llvm): emit closures as env+fnptr structs with internal thunks"
```

---

## Task 9: mvm backend — opcodes, value, and `ELambda`

**Files:**
- Modify: `miva-vm/src/opcode.rs`, `miva-vm/src/value.rs`, `miva-vm/src/vm.rs`, `miva/src/codegen/mvm.rs`

- [ ] **Step 1: Add opcodes in `opcode.rs`** — add after `CallHost = 0xC0` (line 189):

```rust
    /// Build a closure value: pop N captured values (top=N is last capture),
    /// then pop a u32 func index; push Value::Closure(func, env).
    MakeClosure = 0xC1,
    /// Call a closure value: pop args (arity N), pop the Closure, set up its
    /// env as an extra local frame, then jump to its func.
    CallClosure = 0xC2,
```

Add to `from_u8` (after line 297 `0xC0 => Some(CallHost),`): `0xC1 => Some(MakeClosure), 0xC2 => Some(CallClosure),`
Add to `operand_size`: `MakeClosure => 4, CallClosure => 0,`
Add to `name`: `MakeClosure => "make_closure", CallClosure => "call_closure",`

- [ ] **Step 2: Add `Value::Closure`** in `value.rs` (line 22 enum) and its `type_name`/`display` arms:

```rust
    Closure(usize, Arc<Vec<Value>>), // (func index, captured env)
```
In `type_name`: `Value::Closure(_, _) => "closure",`

- [ ] **Step 3: Execute opcodes in `vm.rs`** — add arms for `MakeClosure` (read u32 func idx, collect N captures from stack where N = number of captures for that func — store N alongside, or encode arity in the closure value) and `CallClosure` (load Closure, push env as locals, call func index, restore). Mirror existing `Call` handling in `vm.rs`.

- [ ] **Step 4: Compile `ELambda` in `mvm.rs`** — in `build_ir` pass 1, register each lambda's thunk as an `MvmFunction` (like `DFunc`). In `compile_expr`, add `Expr::ELambda` arm: compile the thunk body (params + env-local captures), and at the use site emit captured vars then `MakeClosure`.

- [ ] **Step 5: Build both crates**

Run: `cd /home/hunter/projects/miva-lang/miva-vm && cargo build && cd /home/hunter/projects/miva-lang/miva && cargo build`
Expected: BUILD success.

- [ ] **Step 6: Commit**

```bash
git add miva-vm/src/opcode.rs miva-vm/src/value.rs miva-vm/src/vm.rs miva/src/codegen/mvm.rs
git commit -m "feat(mvm): add MakeClosure/CallClosure opcodes and closure values"
```

---

## Task 10: End-to-end examples

**Files:**
- Create: `examples/hof/hof.miva`

- [ ] **Step 1: Write a higher-order-function example** exercising closures, captures, and passing closures to functions:

```miva
module hof;

apply = (f: fn(int): int, x: int): int => {
    return f(x);
};

main = () => {
    let n = 10;
    let adder = (m: int): int => { return m + n; };
    print(apply(adder, 5));
    print(adder(3));
    let doubler = (k: int): int => { return k * 2; };
    print(apply(doubler, 21));
    return 0;
};
```

- [ ] **Step 2: Build & run with each backend** (use the project's run command; check `build.sh` / `rtk` targets). Confirm output `15 13 42`.

- [ ] **Step 3: Commit**

```bash
git add examples/hof/hof.miva
git commit -m "examples: add higher-order function and closure demo"
```

---

## Task 11: Typecheck tests

**Files:**
- Create/modify: `miva/test/` (add a typecheck test file following existing test layout — inspect `miva/test/` for the pattern).

- [ ] **Step 1: Add valid + invalid closure tests** — valid: a lambda passed to a higher-order fn; invalid: return-type mismatch, calling a non-function, capture of nonexistent var. Mirror the existing test harness invocation (e.g. a function that runs `check_program` and asserts error count).

- [ ] **Step 2: Run the test suite**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo test`
Expected: All pass.

- [ ] **Step 3: Commit**

```bash
git add miva/test/
git commit -m "test(typecheck): add closure TFunc validation tests"
```

---

## Task 12: Full integration verification

**Files:** none (verification only)

- [ ] **Step 1: Build everything**

Run: `cd /home/hunter/projects/miva-lang && ./build.sh` (or the repo's full build command).
Expected: All crates build.

- [ ] **Step 2: Run frontend parser tests**

Run: `cd /home/hunter/projects/miva-lang/miva-frontend-rs && cargo test`
Expected: PASS.

- [ ] **Step 3: Run compiler tests for all backends**

Run: `cd /home/hunter/projects/miva-lang/miva && cargo test`
Expected: PASS, including the `examples/hof` e2e checks if wired into the harness.

- [ ] **Step 4: Run the `hof` example on cxx, llvm, and mvm** and confirm identical output `15 13 42` on each.

---

## Self-Review Notes

- **Spec coverage:** TFunc type ✓ (Tasks 1,2,4). Lambda syntax `(p:t):ret => {}` ✓ (Task 3). Closure capture env ✓ (Tasks 5,7,8,9). All three backends ✓ (Tasks 7,8,9). Explicit return type required ✓ (Task 5 validation). Testing ✓ (Tasks 10,11,12).
- **Type consistency:** `TFunc { params, returns }` and `ELambda { loc, params, ret, captures, body }` match across frontend/backend AST and all codegen arms.
- **Placeholders:** cxx `miva_closure<...>` template-arg list must be derived from the same `cxx_type(TFunc)` form — keep them in sync during Task 7 implementation.
