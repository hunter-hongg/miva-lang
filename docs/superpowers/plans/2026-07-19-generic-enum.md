# Generic Enum Support Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `Name[T] = enum { A(T), B }` work end-to-end — parse, type-check (explicit `[T]` + inferred), and emit correct code for all three backends (cxx, llvm, mvm).

**Architecture:** The parser and the LLVM/mvm backends are already type-erased and mostly support generic enums with no or trivial changes. The real work is: (1) thread `type_args` through the typechecker's enum map + constructor/pattern sites so `Typ::TStruct{name, type_args}` is populated and compared; (2) make the cxx backend emit a `template<...>` header for generic enums and append `<T>` to constructor calls; (3) a light touch to `semantic.rs` so pattern bindings get the right type.

**Tech Stack:** Rust (miva compiler in `miva/src`, frontend in `miva-frontend-rs/src`), C++ (cxx backend target), LLVM IR (llvm backend target), MVM bytecode (mvm backend target).

---

### File Structure

- Modify `miva/src/typecheck.rs`: `build_enum_map` keeps `type_params`; `infer_type` enum constructor (dotted + desugared) and `EEnumPattern`/`EFieldAccess` discriminant return populated `type_args`; `EChoose` propagates scrutinee `type_args` into pattern types.
- Modify `miva/src/codegen/cxx.rs`: `cxx_enum_def` emits `template<...>` + templated constructors; `cxx_call` enum branch appends `<...>` type args.
- Modify `miva/src/semantic.rs`: bind enum-pattern vars to the resolved payload type (concrete) / `TInt` fallback (generic param).
- Modify `miva-frontend-rs/src/parser.rs`: ensure `enum_pattern_or_call` already threads `type_args` (verify; it does — Task 1 confirms).
- Add `examples/generic-enum.mv`: end-to-end example.
- Tests: `miva-frontend-rs/src/parser.rs` (parser) + `miva/src/typecheck.rs` (typecheck).

---

### Task 1: Verify parser threads type_args into enum constructor ECall

**Files:**
- Read: `miva-frontend-rs/src/parser.rs:931-965,1701-1730`
- Test: `miva-frontend-rs/src/parser.rs` (existing `#[cfg(test)] mod tests`)

- [ ] **Step 1: Add parser test for generic enum definition + constructor type_args**

Add this test inside the existing `mod tests` in `miva-frontend-rs/src/parser.rs` (after `test_parse_enum`):

```rust
#[test]
fn test_parse_generic_enum_constructor_type_args() {
    let def = parse_first("Box = enum { Empty, Value(T) }");
    match def {
        Def::DEnum { name, type_params, variants, .. } => {
            assert_eq!(name, "Box");
            assert_eq!(type_params, vec!["T".to_string()]);
            assert_eq!(variants.len(), 2);
        }
        _ => panic!("expected DEnum"),
    }
    // Explicit type args on a dotted constructor: Box[int].Value(5)
    // parse_call_suffix produces ECall { name: "Box.Value", type_args: [TInt], args: [EInt] }
    let lexer = Lexer::new("Box[int].Value(5)");
    let mut parser = Parser::new(lexer, "Box[int].Value(5)", "test.mv");
    let expr = parser.parse_expr().unwrap();
    match expr {
        Expr::ECall { name, type_args, .. } => {
            assert_eq!(name, "Box.Value");
            assert_eq!(type_args.len(), 1);
        }
        other => panic!("expected ECall, got {:?}", other),
    }
}
```

- [ ] **Step 2: Run test to verify it passes (parser already threads type_args)**

Run: `cd /home/hunter/projects/miva-lang && cargo test -p miva-frontend-rs test_parse_generic_enum_constructor_type_args`

Expected: PASS (parser already passes `type_args` through `enum_pattern_or_call`).

- [ ] **Step 3: Commit**

```bash
git add miva-frontend-rs/src/parser.rs
git commit -m "test: verify generic enum parser threads type_args into ECall"
```

---

### Task 2: Typecheck — keep enum type_params and populate constructor type_args

**Files:**
- Modify: `miva/src/typecheck.rs:292-300` (`build_enum_map`), `miva/src/typecheck.rs:843-913` (`ECall` enum constructors), `miva/src/typecheck.rs:792-842` (`EEnumPattern`), `miva/src/typecheck.rs:1197-1207` (`EFieldAccess` discriminant)
- Test: `miva/src/typecheck.rs` (existing `#[cfg(test)] mod tests`)

- [ ] **Step 1: Write failing typecheck test for generic enum**

Add near the other tests in `miva/src/typecheck.rs`:

```rust
#[test]
fn test_generic_enum_constructor_type_args() {
    let defs = vec![
        make_module("test"),
        Def::DEnum {
            loc: loc(),
            name: "Box".to_string(),
            variants: vec![EnumVariant {
                name: "Value".to_string(),
                payload: vec![Typ::TGenericParam { name: "T".to_string() }],
            }],
            type_params: vec!["T".to_string()],
        },
        make_func(
            "main",
            vec![],
            None,
            Expr::ECall {
                loc: loc(),
                name: "Box.Value".to_string(),
                type_args: vec![Typ::TInt],
                args: vec![Expr::EInt { loc: loc(), value: 5 }],
            },
            Safety::Safe,
        ),
    ];
    let errs = check_program(&defs);
    assert!(errs.is_empty(), "generic enum constructor should typecheck, got: {:?}", errs);
}

#[test]
fn test_generic_enum_constructor_wrong_type() {
    let defs = vec![
        make_module("test"),
        Def::DEnum {
            loc: loc(),
            name: "Box".to_string(),
            variants: vec![EnumVariant {
                name: "Value".to_string(),
                payload: vec![Typ::TGenericParam { name: "T".to_string() }],
            }],
            type_params: vec!["T".to_string()],
        },
        make_func(
            "main",
            vec![],
            None,
            Expr::ECall {
                loc: loc(),
                name: "Box.Value".to_string(),
                type_args: vec![Typ::TInt],
                args: vec![Expr::EBool { loc: loc(), value: true }],
            },
            Safety::Safe,
        ),
    ];
    let errs = check_program(&defs);
    assert!(!errs.is_empty(), "generic enum constructor with wrong payload type should error");
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/hunter/projects/miva-lang && cargo test -p miva test_generic_enum_constructor_type_args`

Expected: `test_generic_enum_constructor_type_args` FAILS (constructor returns `type_args: vec![]` and discards `type_args`, arg-type check uses bare `TGenericParam` which won't equal `TInt`).

- [ ] **Step 3: Update `build_enum_map` to keep type_params**

Replace the function at `miva/src/typecheck.rs:292-300`:

```rust
fn build_enum_map(
    defs: &[Def],
) -> (
    HashMap<String, Vec<crate::ast::EnumVariant>>,
    HashMap<String, Vec<String>>,
) {
    let mut enums = HashMap::new();
    let mut enum_type_params = HashMap::new();
    for def in defs {
        if let Def::DEnum {
            name,
            variants,
            type_params,
            ..
        } = def
        {
            enums.insert(name.clone(), variants.clone());
            enum_type_params.insert(name.clone(), type_params.clone());
        }
    }
    (enums, enum_type_params)
}
```

- [ ] **Step 4: Thread `enum_type_params` through `check_program_with` and `infer_type`**

At `miva/src/typecheck.rs:1667-1668` add `let (enums, enum_type_params) = build_enum_map(defs);` (replacing `let enums = build_enum_map(defs);`).

Add `enum_type_params: &HashMap<String, Vec<String>>` as a parameter to `infer_type` and `require_type` (insert it right after the `enums` parameter in both signatures and all call sites). Pass `&enum_type_params` at every existing `enums` argument in `check_program_with` (lines ~1719-1721, ~1781-1783, ~1809-1811) and in `infer_type`/`require_type` internal recursive calls.

- [ ] **Step 5: Populate constructor type_args in `ECall` (dotted + desugared forms)**

In the dotted enum-constructor branch (`miva/src/typecheck.rs` ~851-877), replace the `for` arg loop and the final `return` so it builds a substitution and returns populated `type_args`. Change the block starting at `if let Some(variants) = enums.get(enum_name) {` to:

```rust
if let Some(variants) = enums.get(enum_name) {
    if let Some(v) = variants.iter().find(|v| v.name == variant_name) {
        let mut errs = Vec::new();
        if v.payload.len() != args.len() {
            errs.push(Error::new("E0016", loc, &format!(
                "enum variant '{}' expects {} argument(s), got {}",
                variant_name, v.payload.len(), args.len())));
        }
        // Build type substitution from enum type params -> caller type args.
        let type_subst: HashMap<String, Typ> = enum_type_params
            .get(enum_name)
            .map(|etp| {
                etp.iter()
                    .enumerate()
                    .filter_map(|(i, tp)| {
                        type_args.get(i).map(|ta| {
                            (tp.clone(), normalize_typ(ta, etp))
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();
        // Infer remaining type params from args when no explicit type args.
        let mut type_subst = type_subst;
        let mut inferred: HashMap<String, Typ> = HashMap::new();
        for (pt, a) in v.payload.iter().zip(args.iter()) {
            let (at, ae) = require_type(
                env, func_sigs, structs, struct_type_params, enums, enum_type_params,
                func_return, a,
            );
            errs.extend(ae);
            if type_subst.is_empty() {
                infer_type_from_arg(pt, &at, &mut inferred);
            }
            let resolved = if type_subst.is_empty() {
                resolve_type(pt, &inferred)
            } else {
                resolve_type(pt, &type_subst)
            };
            if !types_equal(&resolved, &at) {
                errs.push(Error::new("E0014", loc, &format!(
                    "enum variant '{}' argument type mismatch: expected {:?}, got {:?}",
                    variant_name, resolved, at)));
            }
        }
        let normalized_type_args: Vec<Typ> = enum_type_params
            .get(enum_name)
            .map(|etp| {
                if type_args.is_empty() {
                    etp.iter()
                        .map(|tp| inferred.get(tp).cloned().unwrap_or(Typ::TInvalid))
                        .collect()
                } else {
                    type_args.iter().map(|ta| normalize_typ(ta, etp)).collect()
                }
            })
            .unwrap_or_else(|| type_args.clone());
        return (
            Typ::TStruct {
                name: enum_name.to_string(),
                fields: vec![],
                type_args: normalized_type_args,
            },
            errs,
        );
    }
}
```

Apply the same substitution + populated `type_args` logic to the desugared branch (`miva/src/typecheck.rs` ~879-912), where `enum_name` is a `String` (not `&str`) — use `enum_name.as_str()` for map lookups. Replace its `return` `Typ::TStruct { name: enum_name.to_string(), fields: vec![], type_args: vec![] }` with the same populated `type_args` construction (build `type_subst`/`inferred` from `enum_type_params.get(&enum_name)`).

- [ ] **Step 6: Populate type_args in `EEnumPattern` and `EFieldAccess` discriminant**

In `EEnumPattern` (`miva/src/typecheck.rs` ~817-821), the `Some(v)` arm returns `Typ::TStruct { name: enum_name.clone(), fields: vec![], type_args: vec![] }`. Change `type_args: vec![]` to `type_args: enum_type_params.get(enum_name.as_str()).cloned().unwrap_or_default().iter().map(|tp| Typ::TGenericParam { name: tp.clone() }).collect()` so the pattern's enum type carries generic params (matching the constructor type under `types_equal`).

In `EFieldAccess` discriminant (`miva/src/typecheck.rs` ~1201-1203), change `type_args: vec![]` to the same generic-param expansion using `enum_type_params.get(ev_name.as_str())`.

- [ ] **Step 7: Run tests to verify they pass**

Run: `cd /home/hunter/projects/miva-lang && cargo test -p miva type_generic_enum`

Expected: PASS (`test_generic_enum_constructor_type_args` and `test_generic_enum_constructor_wrong_type`). Also run `cargo test -p miva` to ensure no regressions.

- [ ] **Step 8: Commit**

```bash
git add miva/src/typecheck.rs
git commit -m "feat(typecheck): support generic enum type args in constructors and patterns"
```

---

### Task 3: Propagate scrutinee type_args into choose/when pattern type

**Files:**
- Modify: `miva/src/typecheck.rs` `EChoose` handling (~626-790)
- Test: `miva/src/typecheck.rs`

- [ ] **Step 1: Write failing test for choose/when identity with generic enum**

Add:

```rust
#[test]
fn test_generic_enum_choose_identity() {
    let defs = vec![
        make_module("test"),
        Def::DEnum {
            loc: loc(),
            name: "Box".to_string(),
            variants: vec![
                EnumVariant { name: "Empty".to_string(), payload: vec![] },
                EnumVariant {
                    name: "Value".to_string(),
                    payload: vec![Typ::TGenericParam { name: "T".to_string() }],
                },
            ],
            type_params: vec!["T".to_string()],
        },
        make_func(
            "main",
            vec![],
            None,
            Expr::EChoose {
                loc: loc(),
                var: Box::new(Expr::ECall {
                    loc: loc(),
                    name: "Box.Value".to_string(),
                    type_args: vec![Typ::TInt],
                    args: vec![Expr::EInt { loc: loc(), value: 5 }],
                }),
                cases: vec![WhenCase {
                    when: Box::new(Expr::EEnumPattern {
                        loc: loc(),
                        enum_name: "Box".into(),
                        variant: "Value".into(),
                        bindings: vec!["x".into()],
                    }),
                    guard: None,
                    then: Box::new(Expr::EVar { loc: loc(), name: "x".into() }),
                }],
                otherwise: Some(Box::new(Expr::EInt { loc: loc(), value: 0 })),
            },
            Safety::Safe,
        ),
    ];
    let errs = check_program(&defs);
    assert!(errs.is_empty(), "generic enum choose should typecheck, got: {:?}", errs);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /home/hunter/projects/miva-lang && cargo test -p miva test_generic_enum_choose_identity`

Expected: FAILS if `when`-type vs scrutinee-type comparison reports mismatched `type_args` (the scrutinee carries `type_args: [TInt]`, the pattern carries generic params).

- [ ] **Step 3: Propagate scrutinee type_args into the pattern type**

In the `EChoose` handling, after computing `vt` (the scrutinee type) at the top (~632), capture its `type_args`. Then in the `EEnumPattern` matching block (~683-697), when building the pattern's enum type, use the scrutinee's resolved `type_args` instead of the bare generic-param expansion. Concretely, just before the `if let Some(variants) = enums.get(enum_name.as_str())` block, compute:

```rust
let scr_type_args: Vec<Typ> = match &vt {
    Typ::TStruct { type_args, .. } => type_args.clone(),
    _ => vec![],
};
```

and inside the `Some(v)` arm set the pattern var bindings using `v.payload` resolved through a substitution `enum_type_params.get(enum_name) -> scr_type_args` (so `x` gets the concrete type). Also register the binding type as the resolved payload type:

```rust
if let Some(etp) = enum_type_params.get(enum_name.as_str()) {
    let subst: HashMap<String, Typ> = etp.iter()
        .enumerate()
        .filter_map(|(i, tp)| scr_type_args.get(i).map(|ta| (tp.clone(), ta.clone())))
        .collect();
    for (b, bt) in bindings.iter().zip(v.payload.iter()) {
        let resolved = resolve_type(bt, &subst);
        saved_bindings.push((b.clone(), env.vars.insert(b.clone(), resolved)));
    }
}
```

(The `vt`/`wt` `types_equal` comparison at ~673 already compares `TStruct` by name + `type_args`, so once both carry the same `type_args` the branch type matches.)

- [ ] **Step 4: Run tests to verify pass + no regression**

Run: `cd /home/hunter/projects/miva-lang && cargo test -p miva`

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add miva/src/typecheck.rs
git commit -m "feat(typecheck): propagate generic enum type args in choose/when"
```

---

### Task 4: cxx backend — emit template header for generic enums + templated constructors

**Files:**
- Modify: `miva/src/codegen/cxx.rs:766-834` (`cxx_enum_def`), `miva/src/codegen/cxx.rs:388-420` (`cxx_call` enum branch)
- Test: build `examples/generic-enum.mv` with cxx backend (Task 6)

- [ ] **Step 1: Update `cxx_enum_def` to emit template header and templated constructors**

Replace the function body (`miva/src/codegen/cxx.rs:766-834`) with a version that:
  - Computes `template_header` from `type_params` (mirror `cxx_struct_def`: `format!("{}template<{}>\n", ind, params_str)` where each param is `typename P`).
  - Payload field types come from `cxx_type` on normalized variant payloads — for a generic enum, payload `TGenericParam{name:"T"}` renders as `T`, which is exactly the template param. So use `cxx_type` directly on `v.payload[i]` (no change needed to field-type selection, but the `find_map` chooses the first variant with a payload at index `i`; for generic enums ensure that type is the param — it will be, since all variants share the param name `T`).
  - Constructor signature becomes `Name<T> Name_T(...)` (templated return + name) when `!type_params.is_empty()`.
  - Unit discriminant becomes `Name<T> Name_T()`.

Updated `cxx_enum_def`:

```rust
fn cxx_enum_def(
    name: &str,
    type_params: &[String],
    variants: &[crate::ast::EnumVariant],
    ind: String,
    inner: usize,
) -> String {
    let max_fields = variants.iter().map(|v| v.payload.len()).max().unwrap_or(0);
    let mut field_types: Vec<String> = Vec::new();
    for i in 0..max_fields {
        let ty = variants
            .iter()
            .find_map(|v| v.payload.get(i).map(cxx_type))
            .unwrap_or_else(|| "mvp_builtin_int".to_string());
        field_types.push(ty);
    }
    let mut payload_fields = String::new();
    for (i, ty) in field_types.iter().enumerate() {
        payload_fields.push_str(&format!("{}{} field{};\n", indent_str(inner), ty, i));
    }
    let template_header = if type_params.is_empty() {
        String::new()
    } else {
        let params_str = type_params
            .iter()
            .map(|tp| format!("typename {}", tp))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}template<{}>\n", ind, params_str)
    };
    let type_suffix = if type_params.is_empty() {
        String::new()
    } else {
        let args_str = type_params.join(", ");
        format!("<{}>", args_str)
    };
    let struct_str = format!(
        "{}struct {} {{\n{}mvp_builtin_int __tag;\n{}struct {{\n{}{}}} __payload;\n{}bool operator==(const {}& o) const {{ return __tag == o.__tag; }}\n{}bool operator!=(const {}& o) const {{ return __tag != o.__tag; }}\n{}}};\n\n",
        ind, name, indent_str(inner), indent_str(inner), payload_fields, indent_str(inner), indent_str(inner), name, indent_str(inner), name, ind
    );
    let mut ctors = String::new();
    for (idx, v) in variants.iter().enumerate() {
        let params: Vec<String> = v
            .payload
            .iter()
            .enumerate()
            .map(|(i, t)| format!("{} __a{}", cxx_type(t), i))
            .collect();
        let inits: Vec<String> = (0..v.payload.len())
            .map(|i| format!("v.__payload.field{} = __a{};", i, i))
            .collect();
        let ctor = format!(
            "{}inline {} {} {}_{}({}) {{\n{} {} v;\n{} v.__tag = {};\n{} {};\n{} return v;\n{}}}\n\n",
            ind,
            format!("{}{}", name, type_suffix),
            name,
            v.name,
            params.join(", "),
            indent_str(inner),
            format!("{}{}", name, type_suffix),
            indent_str(inner),
            idx,
            indent_str(inner),
            inits.join(&format!("\n{}", indent_str(inner))),
            indent_str(inner),
            ind
        );
        ctors.push_str(&ctor);
    }
    for (idx, v) in variants.iter().enumerate() {
        if v.payload.is_empty() {
            continue;
        }
        let disc = format!(
            "{}inline {} {} {}_{}() {{\n{} {} v;\n{} v.__tag = {};\n{} return v;\n{}}}\n\n",
            ind, format!("{}{}", name, type_suffix), name, v.name, indent_str(inner),
            format!("{}{}", name, type_suffix), indent_str(inner), idx, indent_str(inner), ind
        );
        ctors.push_str(&disc);
    }
    format!("{}{}", template_header, struct_str + &ctors)
}
```

- [ ] **Step 2: Append `<...>` type args in `cxx_call` enum constructor branch**

In `cxx_call` (`miva/src/codegen/cxx.rs:388-420`), the dotted enum-constructor branch returns `format!("{}_{}({})", enum_name, variant, args_strs.join(", "))`. Change it to append the type-arg suffix when `type_args` is non-empty:

```rust
if let Some(dot) = name.find('.') {
    let enum_name = &name[..dot];
    let variant = &name[dot + 1..];
    let suffix = if type_args.is_empty() {
        String::new()
    } else {
        let tas: Vec<_> = type_args.iter().map(cxx_type).collect();
        format!("<{}>", tas.join(", "))
    };
    return format!("{}_{}{}({})", enum_name, variant, suffix, args_strs.join(", "));
}
```

Apply the same `suffix` to the desugared `Circle(Shape, 5)` branch (`enum_name` from `args.first()`): `format!("{}_{}{}({})", enum_name, name, suffix, payload_strs.join(", "))` (compute `suffix` from `type_args` there too).

- [ ] **Step 3: Build-check miva crate**

Run: `cd /home/hunter/projects/miva-lang && cargo build -p miva`

Expected: compiles.

- [ ] **Step 4: Commit**

```bash
git add miva/src/codegen/cxx.rs
git commit -m "feat(cxx): emit template + templated constructors for generic enums"
```

---

### Task 5: semantic.rs — bind enum-pattern vars to resolved payload type

**Files:**
- Modify: `miva/src/semantic.rs:268-313` (enum pattern binding)
- Test: build `examples/generic-enum.mv` with cxx backend (Task 6)

- [ ] **Step 1: Resolve binding type from variant payload when concrete**

In `miva/src/semantic.rs`, inside the `Some(v)` arm of the enum-pattern match (where bindings are inserted with `Typ::TInt`), change the per-binding insertion to use the variant's payload type when it is a concrete type, falling back to `TInt` when it is a generic param:

```rust
for (b, bt) in bindings.iter().zip(v.payload.iter()) {
    let bind_typ = match bt {
        Typ::TGenericParam { .. } => Typ::TInt,
        other => other.clone(),
    };
    ctx.vars.insert(
        b.clone(),
        VarInfo {
            typ: bind_typ,
            state: VarState::Valid,
            is_mutable: false,
            is_ref_param: false,
        },
    );
}
```

(Note: generic-param payloads default to `TInt` to preserve existing type-erased behavior; concrete payload types like `string`/`int` are now correctly bound.)

- [ ] **Step 2: Build-check miva crate**

Run: `cd /home/hunter/projects/miva-lang && cargo build -p miva`

Expected: compiles.

- [ ] **Step 3: Commit**

```bash
git add miva/src/semantic.rs
git commit -m "fix(semantic): bind enum pattern vars to resolved payload type"
```

---

### Task 6: Example + end-to-end build/run on all three backends

**Files:**
- Create: `examples/generic-enum.mv`
- Build/run via `miva` CLI (see `miva --help` / `build.sh`)

- [ ] **Step 1: Write the example**

```miva
Box = enum [T] {
    Empty,
    Value(T),
}

main =
    let b1 = Box[int].Value(42)
    let b2 = Box.Value("hello")

    choose (b1) {
        when (Box.Value(n)) { print(n) }
        when (Box.Empty) { print(0) }
        otherwise { print(-1) }
    }

    choose (b2) {
        when (Box.Value(s)) { prints(s) }
        otherwise { prints("none") }
    }
```

- [ ] **Step 2: Build & run with the cxx backend**

Run: `cd /home/hunter/projects/miva-lang && cargo run -q -p miva -- build examples/generic-enum.mv --backend cxx 2>&1 | tail -30`

Expected: compiles and runs, printing `42` and `hello` (or writes an executable you then run; follow `build.sh` / `miva --help` for the exact run step).

- [ ] **Step 3: Build & run with the llvm backend**

Run: `cd /home/hunter/projects/miva-lang && cargo run -q -p miva -- build examples/generic-enum.mv --backend llvm 2>&1 | tail -30`

Expected: compiles and runs, same output.

- [ ] **Step 4: Build & run with the mvm backend**

Run: `cd /home/hunter/projects/miva-lang && cargo run -q -p miva -- build examples/generic-enum.mv --backend mvm 2>&1 | tail -30`

Expected: compiles and runs, same output.

- [ ] **Step 5: Commit**

```bash
git add examples/generic-enum.mv
git commit -m "example: add generic enum end-to-end example"
```

---

### Task 7: Full test suite + final commit

**Files:**
- Run: whole workspace test + build

- [ ] **Step 1: Run full workspace tests**

Run: `cd /home/hunter/projects/miva-lang && cargo test --workspace 2>&1 | tail -40`

Expected: all pass (frontend parser tests, miva typecheck/semantic tests, no regressions in cxx/llvm/mvm codegen).

- [ ] **Step 2: Run clippy/fmt if configured**

Run: `cd /home/hunter/projects/miva-lang && cargo fmt -p miva -p miva-frontend-rs && cargo clippy -p miva -p miva-frontend-rs 2>&1 | tail -20`

Expected: no new warnings.

- [ ] **Step 3: Commit any final formatting fixes**

```bash
git add -A
git commit -m "chore: formatting and final generic enum support" || echo "nothing to commit"
```
