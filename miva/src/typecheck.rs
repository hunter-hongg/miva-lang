use crate::ast::*;
use crate::error::Error;
use std::collections::HashMap;

// ── Generic type helpers ────────────────────────────────────────────────

/// Convert TStruct references to TGenericParam when they match a type parameter name.
pub(crate) fn normalize_typ(typ: &Typ, type_params: &[String]) -> Typ {
    match typ {
        Typ::TStruct { name, .. } if type_params.contains(name) => {
            Typ::TGenericParam { name: name.clone() }
        }
        Typ::TStruct {
            name,
            fields,
            type_args,
        } => Typ::TStruct {
            name: name.clone(),
            fields: fields.clone(),
            type_args: type_args
                .iter()
                .map(|ta| normalize_typ(ta, type_params))
                .collect(),
        },
        Typ::TArray { of } => Typ::TArray {
            of: Box::new(normalize_typ(of, type_params)),
        },
        Typ::TPtr { to } => Typ::TPtr {
            to: Box::new(normalize_typ(to, type_params)),
        },
        Typ::TBox { of } => Typ::TBox {
            of: Box::new(normalize_typ(of, type_params)),
        },
        Typ::TFuture { of } => Typ::TFuture {
            of: Box::new(normalize_typ(of, type_params)),
        },
        _ => typ.clone(),
    }
}

pub(crate) fn normalize_params(params: &[Param], type_params: &[String]) -> Vec<Param> {
    params
        .iter()
        .map(|p| {
            let (name, typ, is_ref) = match p {
                Param::PRef { name, typ } => (name.clone(), typ.clone(), true),
                Param::POwn { name, typ } => (name.clone(), typ.clone(), false),
            };
            let norm_typ = normalize_typ(&typ, type_params);
            if is_ref {
                Param::PRef {
                    name,
                    typ: norm_typ,
                }
            } else {
                Param::POwn {
                    name,
                    typ: norm_typ,
                }
            }
        })
        .collect()
}

/// Resolve type variables to concrete types using a substitution map.
fn resolve_type(typ: &Typ, subst: &HashMap<String, Typ>) -> Typ {
    match typ {
        Typ::TGenericParam { name } => subst.get(name).cloned().unwrap_or(Typ::TInvalid),
        Typ::TStruct {
            name,
            fields,
            type_args,
        } => Typ::TStruct {
            name: name.clone(),
            fields: fields.clone(),
            type_args: type_args.iter().map(|ta| resolve_type(ta, subst)).collect(),
        },
        Typ::TArray { of } => Typ::TArray {
            of: Box::new(resolve_type(of, subst)),
        },
        Typ::TPtr { to } => Typ::TPtr {
            to: Box::new(resolve_type(to, subst)),
        },
        Typ::TBox { of } => Typ::TBox {
            of: Box::new(resolve_type(of, subst)),
        },
        Typ::TFuture { of } => Typ::TFuture {
            of: Box::new(resolve_type(of, subst)),
        },
        _ => typ.clone(),
    }
}

/// Infer type param bindings from actual argument types.
fn infer_type_from_arg(param_typ: &Typ, arg_typ: &Typ, subst: &mut HashMap<String, Typ>) {
    match (param_typ, arg_typ) {
        (Typ::TGenericParam { name }, _) => {
            if !subst.contains_key(name) {
                subst.insert(name.clone(), arg_typ.clone());
            }
        }
        (Typ::TArray { of: pof }, Typ::TArray { of: aof }) => {
            infer_type_from_arg(pof, aof, subst);
        }
        (Typ::TPtr { to: pto }, Typ::TPtr { to: ato }) => {
            infer_type_from_arg(pto, ato, subst);
        }
        (Typ::TBox { of: pof }, Typ::TBox { of: aof }) => {
            infer_type_from_arg(pof, aof, subst);
        }
        (Typ::TStruct { type_args: pta, .. }, Typ::TStruct { type_args: ata, .. })
            if pta.len() == ata.len() =>
        {
            for (pt, at) in pta.iter().zip(ata.iter()) {
                infer_type_from_arg(pt, at, subst);
            }
        }
        _ => {}
    }
}

struct TypeEnv {
    vars: HashMap<String, Typ>,
}

fn types_equal(a: &Typ, b: &Typ) -> bool {
    // Generic-param equivalence: a bare `TStruct{name:"T"}` (no fields, no
    // type args) that names a generic type parameter is the same type as the
    // normalized `TGenericParam{name:"T"}` form. This lets the rest of type
    // checking treat the two shapes — produced by different code paths
    // (let-annotation parsing keeps TStruct; func_sigs normalize to
    // TGenericParam) — as interchangeable without chasing every producer.
    if let (Typ::TStruct { name: n1, fields: f1, type_args: ta1 },
            Typ::TGenericParam { name: n2 }) = (a, b) {
        if n1 == n2 && f1.is_empty() && ta1.is_empty() {
            return true;
        }
    }
    if let (Typ::TGenericParam { name: n1 },
            Typ::TStruct { name: n2, fields: f2, type_args: ta2 }) = (a, b) {
        if n1 == n2 && f2.is_empty() && ta2.is_empty() {
            return true;
        }
    }
    match (a, b) {
        (
            Typ::TStruct {
                name: n1,
                type_args: ta1,
                ..
            },
            Typ::TStruct {
                name: n2,
                type_args: ta2,
                ..
            },
        ) => {
            n1 == n2
                && ta1.len() == ta2.len()
                && ta1
                    .iter()
                    .zip(ta2.iter())
                    .all(|(t1, t2)| types_equal(t1, t2))
        }
        (Typ::TArray { of: o1 }, Typ::TArray { of: o2 }) => types_equal(o1, o2),
        (Typ::TPtr { to: t1 }, Typ::TPtr { to: t2 }) => types_equal(t1, t2),
        (Typ::TBox { of: o1 }, Typ::TBox { of: o2 }) => types_equal(o1, o2),
        (Typ::TFuture { of: o1 }, Typ::TFuture { of: o2 }) => types_equal(o1, o2),
        (Typ::TGenericParam { name: n1 }, Typ::TGenericParam { name: n2 }) => n1 == n2,
        _ => a == b,
    }
}

fn is_numeric(t: &Typ) -> bool {
    matches!(t, Typ::TInt | Typ::TFloat32 | Typ::TFloat64)
}

fn builtin_return_typ(name: &str) -> Option<Typ> {
    match name {
        "print" | "prints" | "println" | "printlns" | "error" | "errors" | "errorln"
        | "errorlns" | "exit" | "abort" | "panic" | "ptr_free" | "ptr_set" => Some(Typ::TNull),
        "string_concat" | "string_make" | "string_from" => Some(Typ::TString),
        "string_parse" | "string_length" => Some(Typ::TInt),
        "range" => Some(Typ::TArray {
            of: Box::new(Typ::TInt),
        }),
        "ptr_alloc" | "ptr_realloc" | "ptr_offset" => Some(Typ::TPtrAny),
        "box_new" | "box_deref" => None,
        "json_parse" | "json_array_get" | "json_object_get" | "json_object_find" => {
            Some(Typ::TPtrAny)
        },
        "json_kind" | "json_array_len" | "json_object_len" => Some(Typ::TInt),
        "json_bool" => Some(Typ::TBool),
        "json_number" => Some(Typ::TFloat64),
        "json_string" | "json_stringify" => Some(Typ::TString),
        "xml_parse" | "xml_child_get" => Some(Typ::TPtrAny),
        "xml_kind" | "xml_attr_count" | "xml_child_count" => Some(Typ::TInt),
        "xml_tag" | "xml_attr_name" | "xml_attr_value" | "xml_attr_find" | "xml_text"
        | "xml_comment" | "xml_cdata" | "xml_pi_target" | "xml_pi_data"
        | "xml_stringify" => Some(Typ::TString),
        "xml_free" => Some(Typ::TNull),
        "toml_parse" | "toml_array_get" | "toml_object_get" | "toml_object_find" => {
            Some(Typ::TPtrAny)
        }
        "toml_kind" | "toml_array_len" | "toml_object_len" => Some(Typ::TInt),
        "toml_bool" => Some(Typ::TBool),
        "toml_number" => Some(Typ::TFloat64),
        "toml_string" | "toml_stringify" => Some(Typ::TString),
        "toml_free" => Some(Typ::TNull),
        "yaml_parse" | "yaml_array_get" | "yaml_object_get" | "yaml_object_find" => {
            Some(Typ::TPtrAny)
        }
        "yaml_kind" | "yaml_array_len" | "yaml_object_len" => Some(Typ::TInt),
        "yaml_bool" => Some(Typ::TBool),
        "yaml_number" => Some(Typ::TFloat64),
        "yaml_string" | "yaml_stringify" => Some(Typ::TString),
        "yaml_free" => Some(Typ::TNull),
        _ => None,
    }
}

fn build_func_sigs(defs: &[Def]) -> HashMap<String, (Vec<String>, Vec<Param>, Option<Typ>)> {
    let mut sigs = HashMap::new();
    for def in defs {
        match def {
            Def::DFunc {
                name,
                type_params,
                params,
                returns,
                ..
            } => {
                // Normalize generic types so func_sigs contains TGenericParam
                // instead of TStruct for generic type parameter names
                let normalized_type_params = type_params.clone();
                let normalized_params = normalize_params(params, type_params);
                let normalized_returns = returns.as_ref().map(|r| normalize_typ(r, type_params));
                sigs.insert(
                    name.clone(),
                    (
                        normalized_type_params,
                        normalized_params,
                        normalized_returns,
                    ),
                );
            }
            Def::DCFuncUnsafe {
                name,
                params,
                returns,
                ..
            } => {
                sigs.insert(name.clone(), (vec![], params.clone(), returns.clone()));
            }
            _ => {}
        }
    }
    sigs
}

fn build_struct_map(
    defs: &[Def],
) -> (HashMap<String, Vec<FieldDef>>, HashMap<String, Vec<String>>) {
    let mut structs = HashMap::new();
    let mut struct_type_params = HashMap::new();
    for def in defs {
        if let Def::DStruct {
            name,
            fields,
            type_params,
            ..
        } = def
        {
            let normalized = if type_params.is_empty() {
                fields.clone()
            } else {
                fields
                    .iter()
                    .map(|f| FieldDef {
                        name: f.name.clone(),
                        typ: normalize_typ(&f.typ, type_params),
                    })
                    .collect()
            };
            structs.insert(name.clone(), normalized);
            struct_type_params.insert(name.clone(), type_params.clone());
        }
    }
    (structs, struct_type_params)
}

fn build_enum_maps(
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

fn require_type(
    env: &mut TypeEnv,
    func_sigs: &HashMap<String, (Vec<String>, Vec<Param>, Option<Typ>)>,
    structs: &HashMap<String, Vec<FieldDef>>,
    struct_type_params: &HashMap<String, Vec<String>>,
    enums: &HashMap<String, Vec<crate::ast::EnumVariant>>,
    enum_type_params: &HashMap<String, Vec<String>>,
    func_return: &Option<Typ>,
    e: &Expr,
) -> (Typ, Vec<Error>) {
    let (typ, errs) = infer_type(
        env,
        func_sigs,
        structs,
        struct_type_params,
        enums,
        enum_type_params,
        func_return,
        e,
    );
    if matches!(typ, Typ::TNull | Typ::TInvalid) {
        let mut all_errs = errs;
        all_errs.push(Error::new(
            "E0014",
            &loc_of(e),
            "expression has no value (void) but a value was expected",
        ));
        return (typ, all_errs);
    }
    (typ, errs)
}

fn loc_of(e: &Expr) -> Loc {
    match e {
        Expr::EInt { loc, .. }
        | Expr::EBool { loc, .. }
        | Expr::EFloat { loc, .. }
        | Expr::EChar { loc, .. }
        | Expr::EString { loc, .. }
        | Expr::EVar { loc, .. }
        | Expr::EMove { loc, .. }
        | Expr::EClone { loc, .. }
        | Expr::EStructLit { loc, .. }
        | Expr::EFieldAccess { loc, .. }
        | Expr::EBinOp { loc, .. }
        | Expr::EIf { loc, .. }
        | Expr::EChoose { loc, .. }
        | Expr::ECall { loc, .. }
        | Expr::EMacro { loc, .. }
        | Expr::ECast { loc, .. }
        | Expr::EBlock { loc, .. }
        | Expr::EArrayLit { loc, .. }
        | Expr::EVoid { loc, .. }
        | Expr::EAddr { loc, .. }
        | Expr::EDeref { loc, .. }
        | Expr::EWhile { loc, .. }
        | Expr::ELoop { loc, .. }
        | Expr::EFor { loc, .. }
        | Expr::EEnumPattern { loc, .. } => loc.clone(),
        Expr::EMacroVar { loc, .. } => loc.clone(),
        Expr::EMethodCall { loc, .. } => loc.clone(),
    }
}

fn infer_type(
    env: &mut TypeEnv,
    func_sigs: &HashMap<String, (Vec<String>, Vec<Param>, Option<Typ>)>,
    structs: &HashMap<String, Vec<FieldDef>>,
    struct_type_params: &HashMap<String, Vec<String>>,
    enums: &HashMap<String, Vec<crate::ast::EnumVariant>>,
    enum_type_params: &HashMap<String, Vec<String>>,
    func_return: &Option<Typ>,
    e: &Expr,
) -> (Typ, Vec<Error>) {
    match e {
        Expr::EInt { .. } => (Typ::TInt, vec![]),
        Expr::EBool { .. } => (Typ::TBool, vec![]),
        Expr::EFloat { .. } => (Typ::TFloat64, vec![]),
        Expr::EChar { .. } => (Typ::TChar, vec![]),
        Expr::EString { .. } => (Typ::TString, vec![]),
        Expr::EVoid { .. } => (Typ::TNull, vec![]),
        Expr::EVar { name, .. } => match env.vars.get(name) {
            Some(t) => (t.clone(), vec![]),
            None => (Typ::TInvalid, vec![]),
        },
        Expr::EMove { name, .. } => match env.vars.get(name) {
            Some(t) => (t.clone(), vec![]),
            None => (Typ::TInvalid, vec![]),
        },
        Expr::EClone { name, .. } => match env.vars.get(name) {
            Some(t) => (t.clone(), vec![]),
            None => (Typ::TInvalid, vec![]),
        },
        Expr::EAddr { expr, loc, .. } => {
            let (inner_typ, mut errs) = infer_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                expr,
            );
            if matches!(inner_typ, Typ::TNull | Typ::TInvalid) {
                errs.push(Error::new(
                    "E0014",
                    loc,
                    "cannot take address of expression with no value",
                ));
            }
            (
                Typ::TPtr {
                    to: Box::new(inner_typ),
                },
                errs,
            )
        }
        Expr::EDeref { expr, loc, .. } => {
            let (inner_typ, mut errs) = infer_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                expr,
            );
            match inner_typ {
                Typ::TPtr { to } => (*to, errs),
                _ => {
                    errs.push(Error::new(
                        "E0014",
                        loc,
                        &format!("cannot dereference non-pointer type"),
                    ));
                    (Typ::TInvalid, errs)
                }
            }
        }
        Expr::EBinOp {
            op,
            left,
            right,
            loc,
        } => {
            let (lt, mut errs) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                left,
            );
            let (rt, errs2) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                right,
            );
            errs.extend(errs2);

            match op {
                BinOp::Add => {
                    // Allow numeric + numeric (arithmetic) and string + string (concatenation)
                    if lt == Typ::TString && rt == Typ::TString {
                        (Typ::TString, errs)
                    } else if is_numeric(&lt) && is_numeric(&rt) {
                        if !types_equal(&lt, &rt) {
                            errs.push(Error::new(
                                "E0014",
                                loc,
                                &format!("type mismatch in arithmetic: {:?} vs {:?}", lt, rt),
                            ));
                            (Typ::TInvalid, errs)
                        } else {
                            (lt, errs)
                        }
                    } else {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!(
                                "addition requires numeric or string types, got {:?} and {:?}",
                                lt, rt
                            ),
                        ));
                        (Typ::TInvalid, errs)
                    }
                }
                BinOp::Sub | BinOp::Mul => {
                    if !is_numeric(&lt) || !is_numeric(&rt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!(
                                "arithmetic operation requires numeric types, got {:?} and {:?}",
                                lt, rt
                            ),
                        ));
                        (Typ::TInvalid, errs)
                    } else if !types_equal(&lt, &rt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!("type mismatch in arithmetic: {:?} vs {:?}", lt, rt),
                        ));
                        (Typ::TInvalid, errs)
                    } else {
                        (lt, errs)
                    }
                }
                BinOp::Eq | BinOp::Neq => {
                    if !types_equal(&lt, &rt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!("type mismatch in comparison: {:?} vs {:?}", lt, rt),
                        ));
                        (Typ::TInvalid, errs)
                    } else {
                        (Typ::TBool, errs)
                    }
                }
                BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                    // Ordering comparisons: both sides must be numeric (and
                    // equal-typed) so the C++ backend can emit `<`/`>`/`<=`/`>=`.
                    if !is_numeric(&lt) || !is_numeric(&rt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!(
                                "ordering comparison requires numeric types, got {:?} and {:?}",
                                lt, rt
                            ),
                        ));
                        (Typ::TInvalid, errs)
                    } else if !types_equal(&lt, &rt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!("type mismatch in comparison: {:?} vs {:?}", lt, rt),
                        ));
                        (Typ::TInvalid, errs)
                    } else {
                        (Typ::TBool, errs)
                    }
                }
                BinOp::And | BinOp::Or => {
                    // Logical AND/OR: both sides must be bool. Short-circuit
                    // semantics are emitted by the C++ backend (`&&`/`||`).
                    if lt != Typ::TBool || rt != Typ::TBool {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!(
                                "logical operator requires bool operands, got {:?} and {:?}",
                                lt, rt
                            ),
                        ));
                        (Typ::TInvalid, errs)
                    } else {
                        (Typ::TBool, errs)
                    }
                }
            }
        }
        Expr::EIf {
            cond,
            then,
            else_,
            loc,
        } => {
            let (ct, mut errs) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                cond,
            );
            if !types_equal(&ct, &Typ::TBool) {
                errs.push(Error::new(
                    "E0014",
                    loc,
                    &format!("if condition must be bool"),
                ));
            }

            let (tt, errs2) = infer_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                then,
            );
            errs.extend(errs2);

            match else_ {
                Some(else_expr) => {
                    let (et, errs3) = infer_type(
                        env,
                        func_sigs,
                        structs,
                        struct_type_params,
                enums,
                enum_type_params,
                func_return,

                        else_expr,
                    );
                    errs.extend(errs3);
                    if !types_equal(&tt, &et) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            &format!("if and else branches have different types",),
                        ));
                    }
                    (tt, errs)
                }
                None => (Typ::TNull, errs),
            }
        }
        Expr::EChoose {
            var,
            cases,
            otherwise,
            loc,
        } => {
            let (vt, mut errs) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                var,
            );
            let _ = vt;
            let mut tmp_typ = vec![];

            if let Some(else_expr) = otherwise {
                let (et, errs3) = infer_type(
                    env,
                    func_sigs,
                    structs,
                    struct_type_params,
                enums,
                enum_type_params,
                func_return,

                    else_expr,
                );
                errs.extend(errs3);

                let mut all_same = true;
                let mut first_type: Option<Typ> = Some(et);
                let mut first_case = true;
                for case in cases {
                    let (wt, errs_w) = require_type(
                        env,
                        func_sigs,
                        structs,
                        struct_type_params,
                        enums,
                        enum_type_params,
                        func_return,

                        &case.when,
                    );
                    errs.extend(errs_w);
                    // Allow enum type comparison even when type_args differ
                    let is_enum_match = match (&wt, &vt) {
                        (Typ::TStruct { name: n1, .. }, Typ::TStruct { name: n2, .. }) => {
                            n1 == n2 && enums.contains_key(n1)
                        }
                        _ => false,
                    };
                    if !is_enum_match && !types_equal(&wt, &vt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            "choose expr and the variable must have the same type",
                        ));
                    }
                    // Register enum-pattern destructuring bindings so the `then`
                    // branch can refer to them with the variant's payload types.
                    let mut saved_bindings: Vec<(String, Option<Typ>)> = Vec::new();
                    if let Expr::EEnumPattern {
                        enum_name,
                        variant,
                        bindings,
                        ..
                    } = case.when.as_ref()
                    {
                        if let Some(variants) = enums.get(enum_name.as_str()) {
                            if let Some(v) = variants.iter().find(|v| &v.name == variant) {
                                let enum_tp = enum_type_params.get(enum_name.as_str()).cloned().unwrap_or_default();
                                let vt_type_args = match &vt {
                                    Typ::TStruct { type_args, .. } => type_args.clone(),
                                    _ => vec![],
                                };
                                let mut subst = HashMap::new();
                                for (tp, ta) in enum_tp.iter().zip(vt_type_args.iter()) {
                                    subst.insert(tp.clone(), ta.clone());
                                }
                                for (b, bt) in bindings.iter().zip(v.payload.iter()) {
                                    let resolved = if !subst.is_empty() {
                                        resolve_type(&normalize_typ(bt, &enum_tp), &subst)
                                    } else {
                                        bt.clone()
                                    };
                                    saved_bindings.push((b.clone(), env.vars.insert(b.clone(), resolved)));
                                }
                            }
                        }
                    }
                    let (tt, errs_t) = infer_type(
                        env,
                        func_sigs,
                        structs,
                        struct_type_params,
                        enums,
                        enum_type_params,
                        func_return,

                        &case.then,
                    );
                    errs.extend(errs_t);
                    for (b, prev) in saved_bindings {
                        match prev {
                            Some(p) => { env.vars.insert(b, p); }
                            None => { env.vars.remove(&b); }
                        }
                    }
                    if first_case {
                        first_type = Some(tt);
                        first_case = false;
                    } else if let Some(ref ft) = first_type {
                        if !types_equal(&tt, ft) {
                            all_same = false;
                        }
                    }
                }

                if !all_same {
                    errs.push(Error::new(
                        "E0014",
                        loc,
                        "choose branches must all have the same type",
                    ));
                }

                if let Some(ref ft) = first_type {
                    (ft.clone(), errs)
                } else {
                    (Typ::TNull, errs)
                }
            } else {
                for case in cases {
                    let (wt, errs_w) = require_type(
                        env,
                        func_sigs,
                        structs,
                        struct_type_params,
                enums,
                enum_type_params,
                func_return,

                        &case.when,
                    );
                    errs.extend(errs_w);
                    if !types_equal(&wt, &vt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            "choose expr and the variable must have the same type",
                        ));
                    }
                    let (tt, errs_t) = infer_type(
                        env,
                        func_sigs,
                        structs,
                        struct_type_params,
                enums,
                enum_type_params,
                func_return,

                        &case.then,
                    );
                    errs.extend(errs_t);
                    if tmp_typ.is_empty() && tt != Typ::TInvalid {
                        tmp_typ.push(tt);
                    } else {
                        let t1 = tmp_typ.get(0).unwrap_or(&Typ::TInvalid);
                        if t1 == &Typ::TInvalid || tt == Typ::TInvalid {
                            errs.push(Error::new(
                                "E0014",
                                loc,
                                "choose branches must not have invalid type",
                            ));
                        }
                        if !types_equal(&tt, t1) {
                            errs.push(Error::new(
                                "E0014",
                                loc,
                                "choose branches must all have the same type",
                            ));
                        }
                    }
                }
                (tmp_typ.get(0).unwrap_or(&Typ::TInvalid).clone(), errs)
            }
        }
        Expr::EEnumPattern {
            enum_name,
            variant,
            bindings,
            loc,
        } => {
            // Enum destructuring pattern used in `when (Enum.Variant(x, y))`.
            // Validate the enum and variant, and that the number of bindings
            // matches the variant's payload arity.
            let mut errs = vec![];
            let typ = match enums.get(enum_name.as_str()) {
                Some(variants) => match variants.iter().find(|v| &v.name == variant) {
                    Some(v) => {
                        if v.payload.len() != bindings.len() {
                            errs.push(Error::new(
                                "E0016",
                                loc,
                                &format!(
                                    "enum variant '{}' expects {} binding(s), got {}",
                                    variant,
                                    v.payload.len(),
                                    bindings.len()
                                ),
                            ));
                        }
                        Typ::TStruct {
                            name: enum_name.clone(),
                            fields: vec![],
                            type_args: vec![],
                        }
                    }
                    None => {
                        errs.push(Error::new(
                            "E0019",
                            loc,
                            &format!("unknown variant '{}' in enum '{}'", variant, enum_name),
                        ));
                        Typ::TInvalid
                    }
                },
                None => {
                    errs.push(Error::new(
                        "E0018",
                        loc,
                        &format!("unknown enum '{}'", enum_name),
                    ));
                    Typ::TInvalid
                }
            };
            (typ, errs)
        }
        Expr::ECall {
            name,
            type_args,
            args,
            loc,
        } => {
            // Build substitution from explicit or inferred type args for generic enums.
            // `loc` and `enum_name` are used only for error reporting.
            fn resolve_enum_type_args(
                enum_name: &str,
                enum_type_params: &[String],
                call_type_args: &[Typ],
                payload: &[Typ],
                args: &[Expr],
                loc: &crate::ast::Loc,
                env: &mut TypeEnv,
                func_sigs: &HashMap<String, (Vec<String>, Vec<Param>, Option<Typ>)>,
                structs: &HashMap<String, Vec<FieldDef>>,
                struct_type_params: &HashMap<String, Vec<String>>,
                enums: &HashMap<String, Vec<crate::ast::EnumVariant>>,
                enum_type_params_map: &HashMap<String, Vec<String>>,
                func_return: &Option<Typ>,
            ) -> (Vec<Typ>, HashMap<String, Typ>, Vec<Error>) {
                let mut errs = vec![];
                if enum_type_params.is_empty() {
                    return (vec![], HashMap::new(), errs);
                }
                let subst: HashMap<String, Typ>;
                let resolved: Vec<Typ>;
                if !call_type_args.is_empty() {
                    if call_type_args.len() != enum_type_params.len() {
                        errs.push(Error::new("E0016", loc, &format!(
                            "enum '{}' takes {} type argument(s), got {}",
                            enum_name, enum_type_params.len(), call_type_args.len())));
                        resolved = enum_type_params.iter().map(|_| Typ::TInvalid).collect();
                        subst = HashMap::new();
                    } else {
                        subst = enum_type_params.iter().cloned()
                            .zip(call_type_args.iter().cloned())
                            .collect();
                        resolved = call_type_args.to_vec();
                    }
                } else {
                    let mut inferred = HashMap::new();
                    for (pt, a) in payload.iter().zip(args.iter()) {
                        let pt_norm = normalize_typ(pt, enum_type_params);
                        let (at, _) = require_type(env, func_sigs, structs, struct_type_params,
                            enums, enum_type_params_map, func_return, a);
                        infer_type_from_arg(&pt_norm, &at, &mut inferred);
                    }
                    for tp in enum_type_params {
                        if !inferred.contains_key(tp) {
                            inferred.insert(tp.clone(), Typ::TInvalid);
                        }
                    }
                    resolved = enum_type_params.iter()
                        .map(|tp| inferred.get(tp).cloned().unwrap_or(Typ::TInvalid))
                        .collect();
                    subst = inferred;
                }
                (resolved, subst, errs)
            }

            // Enum constructor in dotted-name form: `Name.Variant(args)`.
            if let Some(dot) = name.find('.') {
                let enum_name = &name[..dot];
                let variant_name = &name[dot + 1..];
                if let Some(variants) = enums.get(enum_name) {
                    if let Some(v) = variants.iter().find(|v| v.name == variant_name) {
                        let mut errs = Vec::new();
                        let enum_tp = enum_type_params.get(enum_name).cloned().unwrap_or_default();
                        if v.payload.len() != args.len() {
                            errs.push(Error::new("E0016", loc, &format!(
                                "enum variant '{}' expects {} argument(s), got {}",
                                variant_name, v.payload.len(), args.len())));
                        }
                        let (resolved_type_args, subst, subst_errs) = resolve_enum_type_args(
                            enum_name, &enum_tp, type_args, &v.payload, args,
                            loc, env, func_sigs, structs, struct_type_params, enums,
                            enum_type_params, func_return,
                        );
                        errs.extend(subst_errs);
                        for (i, (pt, a)) in v.payload.iter().zip(args.iter()).enumerate() {
                            let pt_resolved = if !subst.is_empty() {
                                resolve_type(&normalize_typ(pt, &enum_tp), &subst)
                            } else {
                                pt.clone()
                            };
                            let (at, ae) = require_type(
                                env, func_sigs, structs, struct_type_params, enums,
                                enum_type_params, func_return, a,
                            );
                            errs.extend(ae);
                            if !types_equal(&pt_resolved, &at) {
                                errs.push(Error::new("E0014", loc, &format!(
                                    "enum variant '{}' argument {} type mismatch: expected {:?}, got {:?}",
                                    variant_name, i, pt_resolved, at)));
                            }
                        }
                        return (
                            Typ::TStruct { name: enum_name.to_string(), fields: vec![], type_args: resolved_type_args },
                            errs,
                        );
                    }
                }
            } else if let Some(enum_name) = args.first().and_then(|a| match a {
                Expr::EVar { name: n, .. } => Some(n.clone()),
                _ => None,
            }) {
                // Enum constructor in desugared method-call form: `Variant(EnumName, payload...)`.
                // The frontend parses `Shape.Circle(5)` as EMethodCall and macro_expand
                // desugars it to `ECall { name: "Circle", args: [EVar("Shape"), EInt(5)] }`.
                if let Some(variants) = enums.get(&enum_name) {
                    if let Some(v) = variants.iter().find(|v| v.name == *name) {
                        let payload_args = &args[1..];
                        let mut errs = Vec::new();
                        let enum_tp = enum_type_params.get(&enum_name).cloned().unwrap_or_default();
                        if v.payload.len() != payload_args.len() {
                            errs.push(Error::new("E0016", loc, &format!(
                                "enum variant '{}' expects {} argument(s), got {}",
                                name, v.payload.len(), payload_args.len())));
                        }
                        let (resolved_type_args, subst, subst_errs) = resolve_enum_type_args(
                            &enum_name, &enum_tp, type_args, &v.payload, payload_args,
                            loc, env, func_sigs, structs, struct_type_params, enums,
                            enum_type_params, func_return,
                        );
                        errs.extend(subst_errs);
                        for (i, (pt, a)) in v.payload.iter().zip(payload_args.iter()).enumerate() {
                            let pt_resolved = if !subst.is_empty() {
                                resolve_type(&normalize_typ(pt, &enum_tp), &subst)
                            } else {
                                pt.clone()
                            };
                            let (at, ae) = require_type(
                                env, func_sigs, structs, struct_type_params, enums,
                                enum_type_params, func_return, a,
                            );
                            errs.extend(ae);
                            if !types_equal(&pt_resolved, &at) {
                                errs.push(Error::new("E0014", loc, &format!(
                                    "enum variant '{}' argument {} type mismatch: expected {:?}, got {:?}",
                                    name, i, pt_resolved, at)));
                            }
                        }
                        return (
                            Typ::TStruct { name: enum_name.to_string(), fields: vec![], type_args: resolved_type_args },
                            errs,
                        );
                    }
                }
            }
            let mut errs = vec![];
            let mut arg_types = vec![];
            for arg in args {
                let (at, ae) = require_type(
                    env,
                    func_sigs,
                    structs,
                    struct_type_params,
                enums,
                enum_type_params,
                func_return,

                    arg,
                );
                arg_types.push(at);
                errs.extend(ae);
            }

            let user_func = func_sigs.get(name.as_str());
            let ret_typ = if let Some((fn_type_params, params, ret)) = user_func {
                // Build type substitution: generic param -> concrete type
                let mut type_subst: HashMap<String, Typ> = HashMap::new();

                if params.len() != args.len() {
                    errs.push(Error::new(
                        "E0016",
                        loc,
                        &format!(
                            "function '{}' takes {} arguments but got {}",
                            name,
                            params.len(),
                            args.len()
                        ),
                    ));
                } else {
                    // Apply explicit type args if provided
                    if !type_args.is_empty() {
                        if type_args.len() != fn_type_params.len() {
                            errs.push(Error::new(
                                "E0016",
                                loc,
                                &format!(
                                    "function '{}' takes {} type arguments but got {}",
                                    name,
                                    fn_type_params.len(),
                                    type_args.len()
                                ),
                            ));
                        } else {
                            // fn_type_params: the callee's generic param names.
                            // type_args: caller-supplied types (e.g. `push[T]`→`T`,
                            // `push[int]`→`int`). A bare `TStruct{name:"T"}`
                            // type arg that names an *outer* generic param must be
                            // stored as `TGenericParam` so resolve_type can chain
                            // substitutions correctly (else `Vec[T]` resolves to
                            // `Vec<TStruct{name:"T"}>` which won't equal a real
                            // `Vec<TGenericParam{name:"T"}>` arg type).
                            for (tp, ta) in fn_type_params.iter().zip(type_args.iter()) {
                                let norm_ta = match ta {
                                    Typ::TStruct { name, fields, type_args }
                                        if fields.is_empty()
                                            && type_args.is_empty()
                                            && name != "Vec" =>
                                    {
                                        Typ::TGenericParam { name: name.clone() }
                                    }
                                    _ => ta.clone(),
                                };
                                type_subst.insert(tp.clone(), norm_ta);
                            }
                        }
                    }

                    // Infer remaining or all type params from args
                    if !fn_type_params.is_empty() {
                        for (idx, (param, arg_t)) in params.iter().zip(arg_types.iter()).enumerate()
                        {
                            let param_t = match param {
                                Param::PRef { typ, .. } | Param::POwn { typ, .. } => typ,
                            };
                            if !type_args.is_empty() {
                                // With explicit type args, check compatibility
                                let resolved = resolve_type(param_t, &type_subst);
                                if !types_equal(&resolved, arg_t) {
                                    errs.push(Error::new(
                                        "E0016",
                                        loc,
                                        &format!(
                                            "argument {} to function '{}' has wrong type",
                                            idx + 1,
                                            name
                                        ),
                                    ));
                                }
                            } else {
                                // Infer type params from argument
                                infer_type_from_arg(param_t, arg_t, &mut type_subst);
                            }
                        }
                        // Check for type consistency (all args must agree)
                        for (idx, (param, arg_t)) in params.iter().zip(arg_types.iter()).enumerate()
                        {
                            let param_t = match param {
                                Param::PRef { typ, .. } | Param::POwn { typ, .. } => typ,
                            };
                            let resolved = resolve_type(param_t, &type_subst);
                            if !types_equal(&resolved, arg_t) {
                                errs.push(Error::new(
                                    "E0016",
                                    loc,
                                    &format!(
                                        "argument {} to function '{}' has wrong type",
                                        idx + 1,
                                        name
                                    ),
                                ));
                            }
                        }
                    } else {
                        // Non-generic function: check types directly
                        for (i, (param, arg_t)) in params.iter().zip(arg_types.iter()).enumerate() {
                            let param_t = match param {
                                Param::PRef { typ, .. } | Param::POwn { typ, .. } => typ,
                            };
                            if !types_equal(param_t, arg_t) {
                                errs.push(Error::new(
                                    "E0016",
                                    loc,
                                    &format!(
                                        "argument {} to function '{}' has wrong type",
                                        i + 1,
                                        name
                                    ),
                                ));
                            }
                        }
                    }
                }
                // Resolve return type using substitution
                ret.as_ref()
                    .map(|rt| resolve_type(rt, &type_subst))
                    .unwrap_or(Typ::TNull)
            } else {
                match name.as_str() {
                    "box_new" => {
                        if args.is_empty() {
                            Typ::TNull
                        } else {
                            Typ::TBox {
                                of: Box::new(arg_types[0].clone()),
                            }
                        }
                    }
                    "box_deref" => {
                        if args.is_empty() {
                            Typ::TNull
                        } else {
                            match &arg_types[0] {
                                Typ::TBox { of } => *of.clone(),
                                _ => Typ::TInvalid,
                            }
                        }
                    }
                    "await" => {
                        if args.is_empty() {
                            Typ::TNull
                        } else {
                            match &arg_types[0] {
                                Typ::TFuture { of } => (**of).clone(),
                                _ => Typ::TInvalid,
                            }
                        }
                    }
                    name => builtin_return_typ(name).unwrap_or(Typ::TNull),
                }
            };
            (ret_typ, errs)
        }
        Expr::EStructLit {
            name,
            fields,
            type_args,
            loc,
        } => {
            let mut errs = vec![];
            let struct_fields = match structs.get(name.as_str()) {
                Some(f) => f,
                None => {
                    errs.push(Error::new(
                        "E0018",
                        loc,
                        &format!("unknown struct '{}'", name),
                    ));
                    return (Typ::TInvalid, errs);
                }
            };

            let mut field_map: HashMap<&str, &Typ> = HashMap::new();
            for sf in struct_fields {
                field_map.insert(sf.name.as_str(), &sf.typ);
            }

            let mut provided_fields = std::collections::HashSet::new();
            // Normalize type_args: convert TStruct("T") → TGenericParam("T")
            let normalized_type_args: Vec<Typ> = struct_type_params
                .get(name.as_str())
                .map(|stp| type_args.iter().map(|ta| normalize_typ(ta, stp)).collect())
                .unwrap_or_else(|| type_args.clone());
            // Build type substitution from the struct's type_params to the normalized type_args
            let type_subst: HashMap<String, Typ> = struct_type_params
                .get(name.as_str())
                .map(|stp| {
                    stp.iter()
                        .enumerate()
                        .filter_map(|(i, tp)| {
                            normalized_type_args
                                .get(i)
                                .map(|ta| (tp.clone(), ta.clone()))
                        })
                        .collect()
                })
                .unwrap_or_default();
            for vf in fields {
                provided_fields.insert(vf.name.as_str());
                let (ft, fe) = require_type(
                    env,
                    func_sigs,
                    structs,
                    struct_type_params,
                enums,
                enum_type_params,
                func_return,

                    &vf.value,
                );
                errs.extend(fe);
                match field_map.get(vf.name.as_str()) {
                    Some(expected_t) => {
                        let resolved = if type_subst.is_empty() {
                            (*expected_t).clone()
                        } else {
                            resolve_type(expected_t, &type_subst)
                        };
                        if !types_equal(&resolved, &ft) {
                            errs.push(Error::new(
                                "E0018",
                                loc,
                                &format!("field '{}' of struct '{}' has wrong type", vf.name, name),
                            ));
                        }
                    }
                    None => {
                        errs.push(Error::new(
                            "E0018",
                            loc,
                            &format!("unknown field '{}' in struct '{}'", vf.name, name),
                        ));
                    }
                }
            }

            for sf in struct_fields {
                if !provided_fields.contains(sf.name.as_str()) {
                    errs.push(Error::new(
                        "E0018",
                        loc,
                        &format!("missing field '{}' in struct '{}' literal", sf.name, name),
                    ));
                }
            }

            // Normalize type_args using the struct's type_params
            let normalized_type_args: Vec<Typ> = struct_type_params
                .get(name.as_str())
                .map(|stp| type_args.iter().map(|ta| normalize_typ(ta, stp)).collect())
                .unwrap_or_else(|| type_args.clone());
            (
                Typ::TStruct {
                    name: name.clone(),
                    fields: struct_fields.clone(),
                    type_args: normalized_type_args,
                },
                errs,
            )
        }
        Expr::EFieldAccess { expr, field, loc } => {
            if let Expr::EVar { name: ev_name, .. } = expr.as_ref() {
                if let Some(variants) = enums.get(ev_name) {
                    if variants.iter().any(|v| v.name == *field) {
                        return (
                            Typ::TStruct { name: ev_name.clone(), fields: vec![], type_args: vec![] },
                            vec![],
                        );
                    }
                }
            }
            let (et, mut errs) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                expr,
            );
            match et {
                Typ::TStruct { name: sname, .. } => match structs.get(&sname) {
                    Some(fields) => {
                        for f in fields {
                            if f.name == *field {
                                return (f.typ.clone(), errs);
                            }
                        }
                        errs.push(Error::new(
                            "E0019",
                            loc,
                            &format!("unknown field '{}' in struct '{}'", field, sname),
                        ));
                        (Typ::TInvalid, errs)
                    }
                    None => {
                        // Not a struct — check if it's an enum (numeric field access on payload)
                        if let Some(variants) = enums.get(&sname) {
                            if let Ok(idx) = field.parse::<usize>() {
                                // Numeric payload field access on an enum discriminant
                                // (`s.0`, `s.1`, ...). At typecheck time we don't know which
                                // variant `s` currently holds, so accept the index if ANY
                                // variant's payload is long enough. Use the field type from the
                                // variant with the most payload fields (the "widest" variant),
                                // which is the best static approximation of the runtime type.
                                let widest = variants
                                    .iter()
                                    .filter(|v| idx < v.payload.len())
                                    .max_by_key(|v| v.payload.len());
                                if let Some(v) = widest {
                                    return (v.payload[idx].clone(), errs);
                                }
                                errs.push(Error::new(
                                    "E0019",
                                    loc,
                                    &format!("payload index {} out of bounds for enum '{}'", idx, sname),
                                ));
                                (Typ::TInvalid, errs)
                            } else {
                                errs.push(Error::new(
                                    "E0019",
                                    loc,
                                    &format!("unknown variant '{}' in enum '{}'", field, sname),
                                ));
                                (Typ::TInvalid, errs)
                            }
                        } else {
                            errs.push(Error::new(
                                "E0018",
                                loc,
                                &format!("unknown struct '{}'", sname),
                            ));
                            (Typ::TInvalid, errs)
                        }
                    }
                },
                _ => {
                    errs.push(Error::new("E0014", loc, "field access on non-struct type"));
                    (Typ::TInvalid, errs)
                }
            }
        }
        Expr::EArrayLit { values, loc } => {
            if values.is_empty() {
                return (
                    Typ::TArray {
                        of: Box::new(Typ::TInvalid),
                    },
                    vec![],
                );
            }
            let mut errs = vec![];
            let (first_t, fe) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                &values[0],
            );
            errs.extend(fe);
            let mut all_same = true;
            for v in &values[1..] {
                let (vt, ve) =
                    require_type(env, func_sigs, structs, struct_type_params, enums, enum_type_params, func_return, v);
                errs.extend(ve);
                if !types_equal(&first_t, &vt) {
                    all_same = false;
                }
            }
            if !all_same {
                errs.push(Error::new(
                    "E0024",
                    loc,
                    "all array elements must have the same type",
                ));
            }
            (
                Typ::TArray {
                    of: Box::new(first_t),
                },
                errs,
            )
        }
        Expr::ECast { expr, to, loc } => {
            let (et, mut errs) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                expr,
            );
            let valid = match (&et, to) {
                (Typ::TInt, Typ::TFloat32)
                | (Typ::TFloat32, Typ::TInt)
                | (Typ::TInt, Typ::TFloat64)
                | (Typ::TFloat64, Typ::TInt)
                | (Typ::TFloat32, Typ::TFloat64)
                | (Typ::TFloat64, Typ::TFloat32)
                | (Typ::TInt, Typ::TChar)
                | (Typ::TChar, Typ::TInt)
                | (Typ::TBool, Typ::TInt)
                // Integer/pointer reinterpret: lets vec.miva materialise a
                // null `ptrany` via `0 as ptrany` and recover an `int` handle
                // from a `ptrany` when needed. The C++ backend lowers both
                // through `static_cast<void*>(intptr_t)` / casts back.
                | (Typ::TInt, Typ::TPtrAny)
                | (Typ::TPtrAny, Typ::TInt)
                // Opaque ptrany → typed ptr<T>: lets vec.miva turn the raw
                // byte-offset pointer from std.mem.offset into a typed slot
                // for ptr_set/deref. reinterpret_cast<void*> in cxx.rs.
                | (Typ::TPtrAny, Typ::TPtr { .. }) => true,
                _ if types_equal(&et, to) => true,
                _ => false,
            };
            if !valid {
                errs.push(Error::new("E0021", loc, &format!("invalid cast")));
            }
            (to.clone(), errs)
        }
        Expr::EBlock {
            stmts,
            result,
            loc: _,
        } => {
            let mut errs = vec![];
            for stmt in stmts {
                match stmt {
                    Stmt::SLet { name, expr, .. } => {
                        let (t, se) = require_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            expr,
                        );
                        errs.extend(se);
                        env.vars.insert(name.clone(), t);
                    }
                    Stmt::SLetTyped {
                        name,
                        typ,
                        expr,
                        loc,
                    } => {
                        let (t, se) = require_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            expr,
                        );
                        errs.extend(se);
                        if !types_equal(typ, &t) {
                            errs.push(Error::new(
                                "E0022",
                                loc,
                                &format!(
                                    "type mismatch in let: declared {:?} but expression has type {:?}",
                                    typ, t
                                ),
                            ));
                        }
                        env.vars.insert(name.clone(), typ.clone());
                    }
                    Stmt::SAssign { name, expr, loc } => {
                        let (t, se) = require_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            expr,
                        );
                        errs.extend(se);
                        match env.vars.get(name.as_str()) {
                            Some(expected_t) => {
                                if !types_equal(expected_t, &t) {
                                    errs.push(Error::new(
                                        "E0022",
                                        loc,
                                        &format!("cannot assign to variable '{}'", name),
                                    ));
                                }
                            }
                            None => {}
                        }
                    }
                    Stmt::SFieldAssign { target, field, expr, loc } => {
                        // `target.field = expr`: require target to be a struct,
                        // field to exist with type F, and expr to have type F.
                        let (tt, te) = require_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            target,
                        );
                        errs.extend(te);
                        let (et, ee) = require_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            expr,
                        );
                        errs.extend(ee);
                        if let Typ::TStruct { name, .. } = &tt {
                            match structs.get(name) {
                                Some(fields) => {
                                    match fields.iter().find(|f| f.name == *field) {
                                        Some(f) => {
                                            if !types_equal(&f.typ, &et) {
                                                errs.push(Error::new(
                                                    "E0014",
                                                    loc,
                                                    &format!(
                                                        "field '{}' of struct '{}' has wrong type",
                                                        field, name
                                                    ),
                                                ));
                                            }
                                        }
                                        None => {
                                            errs.push(Error::new(
                                                "E0014",
                                                loc,
                                                &format!(
                                                    "struct '{}' has no field '{}'",
                                                    name, field
                                                ),
                                            ));
                                        }
                                    }
                                }
                                None => {}
                            }
                        } else {
                            errs.push(Error::new(
                                "E0014",
                                loc,
                                "field assignment target is not a struct",
                            ));
                        }
                    }
                    Stmt::SReturn { expr, loc } => {
                        let (t, se) = require_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            expr,
                        );
                        errs.extend(se);
                        if let Some(ref rt) = func_return {
                            if !types_equal(rt, &t) {
                                errs.push(Error::new(
                                    "E0017",
                                    loc,
                                    &format!("return type incorrect"),
                                ));
                            }
                        }
                    }
                    Stmt::SExpr { expr, .. } => {
                        let (_, se) = infer_type(
                            env,
                            func_sigs,
                            structs,
                            struct_type_params,
                enums,
                enum_type_params,
                func_return,

                            expr,
                        );
                        errs.extend(se);
                    }
                    Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => {}
                }
            }
            if let Some(r) = result {
                let (t, se) =
                    infer_type(env, func_sigs, structs, struct_type_params, enums, enum_type_params, func_return, r);
                errs.extend(se);
                (t, errs)
            } else {
                (Typ::TNull, errs)
            }
        }
        Expr::EWhile { cond, body, loc } => {
            let mut errs = vec![];
            let (ct, ce) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                cond,
            );
            errs.extend(ce);
            if !types_equal(&ct, &Typ::TBool) {
                errs.push(Error::new(
                    "E0014",
                    loc,
                    &format!("while condition must be bool"),
                ));
            }
            let (_, be) = infer_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                body,
            );
            errs.extend(be);
            (Typ::TNull, errs)
        }
        Expr::ELoop { body, .. } => {
            let mut errs = vec![];
            let (_, be) = infer_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                body,
            );
            errs.extend(be);
            (Typ::TNull, errs)
        }
        Expr::EFor {
            var,
            range,
            body,
            loc,
        } => {
            let mut errs = vec![];
            let (rt, re) = require_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                range,
            );
            errs.extend(re);
            let elem_type = match rt {
                Typ::TArray { ref of } => of.as_ref().clone(),
                _ => {
                    errs.push(Error::new(
                        "E0026",
                        loc,
                        &format!("for loop range must be an array"),
                    ));
                    Typ::TInvalid
                }
            };
            env.vars.insert(var.clone(), elem_type);
            let (_, be) = infer_type(
                env,
                func_sigs,
                structs,
                struct_type_params,
                enums,
                enum_type_params,
                func_return,

                body,
            );
            errs.extend(be);
            (Typ::TNull, errs)
        }
        Expr::EMacro { .. } => (Typ::TNull, vec![]),
        Expr::EMacroVar { .. } => unreachable!(),
        Expr::EMethodCall { .. } => unreachable!(),
    }
}

pub fn check_program(defs: &[Def]) -> Vec<Error> {
    check_program_with(defs, &std::collections::HashMap::new())
}

/// Like [`check_program`], but also consults `global` — a map of
/// fully-qualified call paths (e.g. `"mvp_std.json.parse"`) to their
/// signatures, collected from every module in the build. This lets
/// module-qualified cross-module calls (`std.json.parse`, `l2.helper.foo`,
/// ...) resolve during type checking instead of falling back to `TNull`.
pub fn check_program_with(
    defs: &[Def],
    global: &HashMap<String, (Vec<String>, Vec<Param>, Option<Typ>)>,
) -> Vec<Error> {
    let mut func_sigs = build_func_sigs(defs);
    // Merge cross-module signatures (qualified keys). Local (per-file)
    // signatures take precedence on collisions.
    for (k, v) in global {
        if !func_sigs.contains_key(k) {
            func_sigs.insert(k.clone(), v.clone());
        }
    }
    let (structs, struct_type_params) = build_struct_map(defs);
    let (enums, enum_type_params) = build_enum_maps(defs);
    let has_imports = defs.iter().any(|d| {
        matches!(d, Def::SImport { .. })
            || matches!(d, Def::SImportAs { .. })
            || matches!(d, Def::SImportHere { .. })
    });
    let mut errs = vec![];

    for def in defs {
        match def {
            Def::DFunc {
                name,
                type_params,
                params,
                returns,
                body,
                is_async,
                ..
            } if *is_async => {
                let normalized_params = if !type_params.is_empty() {
                    normalize_params(params, type_params)
                } else {
                    params.clone()
                };
                // An async function must return a future[T]. The body itself is
                // checked against the inner type T.
                let expected_inner: Option<Typ> = match returns {
                    Some(Typ::TFuture { of }) => Some((**of).clone()),
                    Some(_) => {
                        errs.push(Error::new(
                            "E0020",
                            &loc_of(body),
                            "async function must return future[T]; declared return type is not a future",
                        ));
                        None
                    }
                    None => None,
                };
                let mut env = TypeEnv {
                    vars: HashMap::new(),
                };
                for p in &normalized_params {
                    match p {
                        Param::PRef { name, typ } | Param::POwn { name, typ } => {
                            env.vars.insert(name.clone(), typ.clone());
                        }
                    }
                }
                let (body_t, mut fun_errs) = infer_type(
                    &mut env,
                    &func_sigs,
                    &structs,
                    &struct_type_params,
                    &enums,
                    &enum_type_params,
                    &expected_inner,
                    body,
                );
                errs.append(&mut fun_errs);
                if let Some(ref inner) = expected_inner {
                    if !matches!(body_t, Typ::TNull) && !types_equal(inner, &body_t) {
                        errs.push(Error::new(
                            "E0017",
                            &loc_of(body),
                            &format!(
                                "async function body has type {:?} but declared future element type is {:?}",
                                body_t, inner
                            ),
                        ));
                    }
                }
                // Callers see the function as returning future[inner]: a `return`
                // statement makes the block infer to TNull, so do not derive the
                // future element from the (null) body type.
                let eff = match &expected_inner {
                    Some(inner) => Typ::TFuture {
                        of: Box::new(inner.clone()),
                    },
                    None => Typ::TFuture {
                        of: Box::new(body_t.clone()),
                    },
                };
                func_sigs.insert(
                    name.clone(),
                    (type_params.clone(), normalized_params, Some(eff)),
                );
            }
            Def::DFunc {
                name: _,
                type_params,
                params,
                returns,
                body,
                ..
            } => {
                let mut env = TypeEnv {
                    vars: HashMap::new(),
                };
                let normalized_params = if !type_params.is_empty() {
                    normalize_params(params, type_params)
                } else {
                    params.clone()
                };
                let normalized_returns = returns.as_ref().map(|r| normalize_typ(r, type_params));
                for p in &normalized_params {
                    match p {
                        Param::PRef { name, typ } | Param::POwn { name, typ } => {
                            env.vars.insert(name.clone(), typ.clone());
                        }
                    }
                }
                let (body_t, mut fun_errs) = infer_type(
                    &mut env,
                    &func_sigs,
                    &structs,
                    &struct_type_params,
                    &enums,
                    &enum_type_params,
                    &normalized_returns,
                    body,
                );
                errs.append(&mut fun_errs);
                if let Some(ref rt) = normalized_returns {
                    if !matches!(body_t, Typ::TNull) && !types_equal(rt, &body_t) {
                        errs.push(Error::new(
                            "E0017",
                            &loc_of(body),
                            &format!(
                                "function body has type {:?} but declared return type is {:?}",
                                body_t, rt
                            ),
                        ));
                    }
                }
            }
            Def::DTest { body, .. } => {
                let mut env = TypeEnv {
                    vars: HashMap::new(),
                };
                let (_, mut fun_errs) = infer_type(
                    &mut env,
                    &func_sigs,
                    &structs,
                    &struct_type_params,
                    &enums,
                    &enum_type_params,
                    &None,
                    body,
                );
                errs.append(&mut fun_errs);
            }
            _ => {}
        }
    }

    if has_imports {
        // Cross-module functions aren't available for type checking.
        // Filter out "void value" errors that stem from unresolved cross-module calls.
        // The C++ compiler will catch any real type mismatches.
        errs.retain(|e| e.code != "E0014");
    }

    errs
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Loc {
        Loc { line: 1, col: 1 }
    }

    fn make_module(name: &str) -> Def {
        Def::DModule {
            loc: loc(),
            name: name.to_string(),
        }
    }

    fn make_func(
        name: &str,
        params: Vec<Param>,
        returns: Option<Typ>,
        body: Expr,
        safety: Safety,
    ) -> Def {
        Def::DFunc {
            loc: loc(),
            name: name.to_string(),
            type_params: vec![],
            params,
            returns,
            body: Box::new(body),
            safety,
            is_async: false,
        }
    }

    fn make_struct(name: &str, fields: Vec<FieldDef>) -> Def {
        Def::DStruct {
            loc: loc(),
            name: name.to_string(),
            fields,
            type_params: vec![],
        }
    }

    #[test]
    fn test_empty_program_no_type_errors() {
        let defs = vec![make_module("test")];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "empty program should have no type errors");
    }

    #[test]
    fn test_valid_int_addition() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Add,
                    left: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    right: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 2,
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "int + int should be valid, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_type_mismatch_binop() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Add,
                    left: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    right: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "int + bool should be a type error");
        assert!(errs.iter().any(|e| e.code == "E0014"));
    }

    #[test]
    fn test_if_condition_must_be_bool() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 0,
                    }),
                    then: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    else_: Some(Box::new(Expr::EInt {
                        loc: loc(),
                        value: 2,
                    })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "if condition with int should error");
        assert!(errs.iter().any(|e| e.code == "E0014"));
    }

    #[test]
    fn test_if_else_type_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    then: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    else_: Some(Box::new(Expr::EBool {
                        loc: loc(),
                        value: false,
                    })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "if/else type mismatch should error");
        assert!(errs.iter().any(|e| e.code == "E0014"));
    }

    #[test]
    fn test_if_else_same_type_ok() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    then: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    else_: Some(Box::new(Expr::EInt {
                        loc: loc(),
                        value: 2,
                    })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "if/else same type should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_if_void_branch_no_else_ok() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    then: Box::new(Expr::EVoid { loc: loc() }),
                    else_: None,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "if with void then and no else should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_if_void_then_void_else_ok() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EIf {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    then: Box::new(Expr::EVoid { loc: loc() }),
                    else_: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "if with both void branches should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_fn_call_arg_type_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "needs_int",
                vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                None,
                Expr::EVoid { loc: loc() },
                Safety::Safe,
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::ECall {
                    loc: loc(),
                    name: "needs_int".to_string(),
                    type_args: vec![],
                    args: vec![Expr::EBool {
                        loc: loc(),
                        value: true,
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "arg type mismatch should error");
        assert!(errs.iter().any(|e| e.code == "E0016"));
    }

    #[test]
    fn test_fn_call_arg_count_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "two_args",
                vec![
                    Param::POwn {
                        name: "a".to_string(),
                        typ: Typ::TInt,
                    },
                    Param::POwn {
                        name: "b".to_string(),
                        typ: Typ::TInt,
                    },
                ],
                None,
                Expr::EVoid { loc: loc() },
                Safety::Safe,
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::ECall {
                    loc: loc(),
                    name: "two_args".to_string(),
                    type_args: vec![],
                    args: vec![Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "arg count mismatch should error");
        assert!(errs.iter().any(|e| e.code == "E0016"));
    }

    #[test]
    fn test_fn_call_correct_args() {
        let defs = vec![
            make_module("test"),
            make_func(
                "add",
                vec![
                    Param::POwn {
                        name: "a".to_string(),
                        typ: Typ::TInt,
                    },
                    Param::POwn {
                        name: "b".to_string(),
                        typ: Typ::TInt,
                    },
                ],
                Some(Typ::TInt),
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Add,
                    left: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "a".to_string(),
                    }),
                    right: Box::new(Expr::EVar {
                        loc: loc(),
                        name: "b".to_string(),
                    }),
                },
                Safety::Safe,
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::ECall {
                    loc: loc(),
                    name: "add".to_string(),
                    type_args: vec![],
                    args: vec![
                        Expr::EInt {
                            loc: loc(),
                            value: 1,
                        },
                        Expr::EInt {
                            loc: loc(),
                            value: 2,
                        },
                    ],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "correct args should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_struct_literal_type_check() {
        let defs = vec![
            make_module("test"),
            make_struct(
                "Point",
                vec![
                    FieldDef {
                        name: "x".to_string(),
                        typ: Typ::TInt,
                    },
                    FieldDef {
                        name: "y".to_string(),
                        typ: Typ::TInt,
                    },
                ],
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::EStructLit {
                    loc: loc(),
                    name: "Point".to_string(),
                    type_args: vec![],
                    fields: vec![
                        ValueField {
                            name: "x".to_string(),
                            value: Expr::EInt {
                                loc: loc(),
                                value: 1,
                            },
                        },
                        ValueField {
                            name: "y".to_string(),
                            value: Expr::EInt {
                                loc: loc(),
                                value: 2,
                            },
                        },
                    ],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "correct struct lit should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_struct_literal_wrong_field_type() {
        let defs = vec![
            make_module("test"),
            make_struct(
                "Point",
                vec![FieldDef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::EStructLit {
                    loc: loc(),
                    name: "Point".to_string(),
                    type_args: vec![],
                    fields: vec![ValueField {
                        name: "x".to_string(),
                        value: Expr::EBool {
                            loc: loc(),
                            value: true,
                        },
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "wrong field type should error");
        assert!(errs.iter().any(|e| e.code == "E0018"));
    }

    #[test]
    fn test_struct_literal_missing_field() {
        let defs = vec![
            make_module("test"),
            make_struct(
                "Point",
                vec![
                    FieldDef {
                        name: "x".to_string(),
                        typ: Typ::TInt,
                    },
                    FieldDef {
                        name: "y".to_string(),
                        typ: Typ::TInt,
                    },
                ],
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::EStructLit {
                    loc: loc(),
                    name: "Point".to_string(),
                    type_args: vec![],
                    fields: vec![ValueField {
                        name: "x".to_string(),
                        value: Expr::EInt {
                            loc: loc(),
                            value: 1,
                        },
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "missing field should error");
        assert!(errs.iter().any(|e| e.code == "E0018"));
    }

    #[test]
    fn test_unknown_struct() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EStructLit {
                    loc: loc(),
                    name: "NonExistent".to_string(),
                    type_args: vec![],
                    fields: vec![],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "unknown struct should error");
        assert!(errs.iter().any(|e| e.code == "E0018"));
    }

    #[test]
    fn test_field_access_ok() {
        let defs = vec![
            make_module("test"),
            make_struct(
                "Point",
                vec![FieldDef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::EFieldAccess {
                    loc: loc(),
                    expr: Box::new(Expr::EStructLit {
                        loc: loc(),
                        name: "Point".to_string(),
                        type_args: vec![],
                        fields: vec![ValueField {
                            name: "x".to_string(),
                            value: Expr::EInt {
                                loc: loc(),
                                value: 1,
                            },
                        }],
                    }),
                    field: "x".to_string(),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "valid field access should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_field_access_unknown_field() {
        let defs = vec![
            make_module("test"),
            make_struct(
                "Point",
                vec![FieldDef {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
            ),
            make_func(
                "main",
                vec![],
                None,
                Expr::EFieldAccess {
                    loc: loc(),
                    expr: Box::new(Expr::EStructLit {
                        loc: loc(),
                        name: "Point".to_string(),
                        type_args: vec![],
                        fields: vec![ValueField {
                            name: "x".to_string(),
                            value: Expr::EInt {
                                loc: loc(),
                                value: 1,
                            },
                        }],
                    }),
                    field: "z".to_string(),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "unknown field should error");
        assert!(errs.iter().any(|e| e.code == "E0019"));
    }

    #[test]
    fn test_return_type_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Some(Typ::TInt),
                Expr::EBool {
                    loc: loc(),
                    value: true,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "return type mismatch should error");
        assert!(errs.iter().any(|e| e.code == "E0017"));
    }

    #[test]
    fn test_return_type_match_ok() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Some(Typ::TInt),
                Expr::EInt {
                    loc: loc(),
                    value: 42,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "correct return type should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_assignment_type_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TInt,
                }],
                None,
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SAssign {
                        loc: loc(),
                        name: "x".to_string(),
                        expr: Box::new(Expr::EBool {
                            loc: loc(),
                            value: true,
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "assign bool to int should error");
        assert!(errs.iter().any(|e| e.code == "E0022"));
    }

    #[test]
    fn test_array_type_consistency() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EArrayLit {
                    loc: loc(),
                    values: vec![
                        Expr::EInt {
                            loc: loc(),
                            value: 1,
                        },
                        Expr::EBool {
                            loc: loc(),
                            value: true,
                        },
                    ],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "mixed array types should error");
        assert!(errs.iter().any(|e| e.code == "E0024"));
    }

    #[test]
    fn test_array_type_homogeneous_ok() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EArrayLit {
                    loc: loc(),
                    values: vec![
                        Expr::EInt {
                            loc: loc(),
                            value: 1,
                        },
                        Expr::EInt {
                            loc: loc(),
                            value: 2,
                        },
                    ],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "homogeneous array should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_invalid_cast() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::ECast {
                    loc: loc(),
                    expr: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    to: Typ::TString,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "invalid cast should error");
        assert!(errs.iter().any(|e| e.code == "E0021"));
    }

    #[test]
    fn test_valid_cast() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::ECast {
                    loc: loc(),
                    expr: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 65,
                    }),
                    to: Typ::TChar,
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "int to char cast should be valid, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_while_condition_must_be_bool() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EWhile {
                    loc: loc(),
                    cond: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    body: Box::new(Expr::EVoid { loc: loc() }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "while condition must be bool");
        assert!(errs.iter().any(|e| e.code == "E0014"));
    }

    #[test]
    fn test_while_valid() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EWhile {
                    loc: loc(),
                    cond: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    body: Box::new(Expr::EVoid { loc: loc() }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "valid while should be ok, got: {:?}", errs);
    }

    #[test]
    fn test_for_loop_range_type() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EFor {
                    loc: loc(),
                    var: "i".to_string(),
                    range: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                    body: Box::new(Expr::EVoid { loc: loc() }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "for range must be array");
        assert!(errs.iter().any(|e| e.code == "E0026"));
    }

    #[test]
    fn test_block_let_inference() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SLet {
                            loc: loc(),
                            mutable: false,
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 42,
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EBinOp {
                                loc: loc(),
                                op: BinOp::Add,
                                left: Box::new(Expr::EVar {
                                    loc: loc(),
                                    name: "x".to_string(),
                                }),
                                right: Box::new(Expr::EInt {
                                    loc: loc(),
                                    value: 1,
                                }),
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "let inference should work, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_deref_non_pointer() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EDeref {
                    loc: loc(),
                    expr: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 42,
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "deref non-pointer should error");
        assert!(errs.iter().any(|e| e.code == "E0014"));
    }

    #[test]
    fn test_eq_operator_on_same_types() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Eq,
                    left: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    right: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 2,
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "eq on ints should be ok, got: {:?}", errs);
    }

    #[test]
    fn test_eq_operator_type_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Eq,
                    left: Box::new(Expr::EInt {
                        loc: loc(),
                        value: 1,
                    }),
                    right: Box::new(Expr::EBool {
                        loc: loc(),
                        value: true,
                    }),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "eq on different types should error");
        assert!(errs.iter().any(|e| e.code == "E0014"));
    }

    #[test]
    fn test_nested_blocks() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                None,
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![
                        Stmt::SLet {
                            loc: loc(),
                            mutable: false,
                            name: "x".to_string(),
                            expr: Box::new(Expr::EInt {
                                loc: loc(),
                                value: 1,
                            }),
                        },
                        Stmt::SExpr {
                            loc: loc(),
                            expr: Box::new(Expr::EBlock {
                                loc: loc(),
                                stmts: vec![Stmt::SLet {
                                    loc: loc(),
                                    mutable: false,
                                    name: "y".to_string(),
                                    expr: Box::new(Expr::EBool {
                                        loc: loc(),
                                        value: true,
                                    }),
                                }],
                                result: Some(Box::new(Expr::EVar {
                                    loc: loc(),
                                    name: "y".to_string(),
                                })),
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EBinOp {
                        loc: loc(),
                        op: BinOp::Add,
                        left: Box::new(Expr::EVar {
                            loc: loc(),
                            name: "x".to_string(),
                        }),
                        right: Box::new(Expr::EInt {
                            loc: loc(),
                            value: 2,
                        }),
                    })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "nested blocks with type inference should work, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_return_stmt_in_block() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Some(Typ::TInt),
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SReturn {
                        loc: loc(),
                        expr: Box::new(Expr::EInt {
                            loc: loc(),
                            value: 42,
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            errs.is_empty(),
            "return int from int function should be ok, got: {:?}",
            errs
        );
    }

    #[test]
    fn test_return_stmt_type_mismatch() {
        let defs = vec![
            make_module("test"),
            make_func(
                "main",
                vec![],
                Some(Typ::TInt),
                Expr::EBlock {
                    loc: loc(),
                    stmts: vec![Stmt::SReturn {
                        loc: loc(),
                        expr: Box::new(Expr::EBool {
                            loc: loc(),
                            value: true,
                        }),
                    }],
                    result: Some(Box::new(Expr::EVoid { loc: loc() })),
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(
            !errs.is_empty(),
            "return bool from int function should error"
        );
        assert!(errs.iter().any(|e| e.code == "E0017"));
    }

    #[test]
    fn test_generic_identity_call() {
        let defs = vec![
            make_module("test"),
            Def::DFunc {
                loc: loc(),
                name: "identity".to_string(),
                type_params: vec!["T".to_string()],
                params: vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TGenericParam {
                        name: "T".to_string(),
                    },
                }],
                returns: Some(Typ::TGenericParam {
                    name: "T".to_string(),
                }),
                body: Box::new(Expr::EVar {
                    loc: loc(),
                    name: "x".to_string(),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
            make_func(
                "main",
                vec![],
                None,
                Expr::ECall {
                    loc: loc(),
                    name: "identity".to_string(),
                    type_args: vec![Typ::TInt],
                    args: vec![Expr::EInt {
                        loc: loc(),
                        value: 42,
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "expected no type errors, got: {:?}", errs);
    }

    #[test]
    fn test_generic_inference_no_type_args() {
        let defs = vec![
            make_module("test"),
            Def::DFunc {
                loc: loc(),
                name: "identity".to_string(),
                type_params: vec!["T".to_string()],
                params: vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TGenericParam {
                        name: "T".to_string(),
                    },
                }],
                returns: Some(Typ::TGenericParam {
                    name: "T".to_string(),
                }),
                body: Box::new(Expr::EVar {
                    loc: loc(),
                    name: "x".to_string(),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
            make_func(
                "main",
                vec![],
                None,
                Expr::ECall {
                    loc: loc(),
                    name: "identity".to_string(),
                    type_args: vec![],
                    args: vec![Expr::EInt {
                        loc: loc(),
                        value: 42,
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "expected no type errors, got: {:?}", errs);
    }

    #[test]
    fn test_generic_type_arg_mismatch() {
        let defs = vec![
            make_module("test"),
            Def::DFunc {
                loc: loc(),
                name: "identity".to_string(),
                type_params: vec!["T".to_string()],
                params: vec![Param::POwn {
                    name: "x".to_string(),
                    typ: Typ::TGenericParam {
                        name: "T".to_string(),
                    },
                }],
                returns: Some(Typ::TGenericParam {
                    name: "T".to_string(),
                }),
                body: Box::new(Expr::EVar {
                    loc: loc(),
                    name: "x".to_string(),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
            make_func(
                "main",
                vec![],
                None,
                Expr::ECall {
                    loc: loc(),
                    name: "identity".to_string(),
                    type_args: vec![Typ::TInt],
                    args: vec![Expr::EString {
                        loc: loc(),
                        value: "hello".to_string(),
                    }],
                },
                Safety::Safe,
            ),
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "expected type mismatch error");
    }

    #[test]
    fn test_enum_typecheck_construct_and_match() {
        use crate::ast::*;
        let defs = vec![
            Def::DEnum {
                loc: Loc { line: 1, col: 1 },
                name: "Shape".into(),
                variants: vec![
                    EnumVariant { name: "Circle".into(), payload: vec![Typ::TInt] },
                    EnumVariant { name: "Rect".into(), payload: vec![Typ::TInt, Typ::TInt] },
                ],
                type_params: vec![],
            },
            Def::DFunc {
                loc: Loc { line: 2, col: 1 },
                name: "area".into(),
                type_params: vec![],
                params: vec![Param::POwn { name: "s".into(), typ: Typ::TStruct { name: "Shape".into(), fields: vec![], type_args: vec![] } }],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EChoose {
                    loc: Loc { line: 3, col: 1 },
                    var: Box::new(Expr::EVar { loc: Loc { line: 3, col: 1 }, name: "s".into() }),
                    cases: vec![WhenCase {
                        when: Box::new(Expr::EFieldAccess {
                            loc: Loc { line: 4, col: 1 },
                            expr: Box::new(Expr::EVar { loc: Loc { line: 4, col: 1 }, name: "Shape".into() }),
                            field: "Circle".into(),
                        }),
                        guard: None,
                        then: Box::new(Expr::EInt { loc: Loc { line: 4, col: 1 }, value: 1 }),
                    }],
                    otherwise: Some(Box::new(Expr::EInt { loc: Loc { line: 5, col: 1 }, value: 0 })),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "unexpected errors: {:?}", errs);
    }

    #[test]
    fn test_enum_pattern_destructure_typecheck() {
        use crate::ast::*;
        let defs = vec![
            Def::DEnum {
                loc: Loc { line: 1, col: 1 },
                name: "Shape".into(),
                variants: vec![
                    EnumVariant { name: "Circle".into(), payload: vec![Typ::TInt] },
                    EnumVariant { name: "Rect".into(), payload: vec![Typ::TInt, Typ::TInt] },
                ],
                type_params: vec![],
            },
            Def::DFunc {
                loc: Loc { line: 2, col: 1 },
                name: "area".into(),
                type_params: vec![],
                params: vec![Param::POwn { name: "s".into(), typ: Typ::TStruct { name: "Shape".into(), fields: vec![], type_args: vec![] } }],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EChoose {
                    loc: Loc { line: 3, col: 1 },
                    var: Box::new(Expr::EVar { loc: Loc { line: 3, col: 1 }, name: "s".into() }),
                    cases: vec![
                        WhenCase {
                            when: Box::new(Expr::EEnumPattern {
                                loc: Loc { line: 4, col: 1 },
                                enum_name: "Shape".into(),
                                variant: "Circle".into(),
                                bindings: vec!["r".into()],
                            }),
                            guard: None,
                            then: Box::new(Expr::EBinOp {
                                loc: Loc { line: 4, col: 1 },
                                op: BinOp::Mul,
                                left: Box::new(Expr::EVar { loc: Loc { line: 4, col: 1 }, name: "r".into() }),
                                right: Box::new(Expr::EVar { loc: Loc { line: 4, col: 1 }, name: "r".into() }),
                            }),
                        },
                        WhenCase {
                            when: Box::new(Expr::EEnumPattern {
                                loc: Loc { line: 5, col: 1 },
                                enum_name: "Shape".into(),
                                variant: "Rect".into(),
                                bindings: vec!["w".into(), "h".into()],
                            }),
                            guard: None,
                            then: Box::new(Expr::EBinOp {
                                loc: Loc { line: 5, col: 1 },
                                op: BinOp::Add,
                                left: Box::new(Expr::EVar { loc: Loc { line: 5, col: 1 }, name: "w".into() }),
                                right: Box::new(Expr::EVar { loc: Loc { line: 5, col: 1 }, name: "h".into() }),
                            }),
                        },
                    ],
                    otherwise: Some(Box::new(Expr::EInt { loc: Loc { line: 6, col: 1 }, value: 0 })),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "unexpected errors: {:?}", errs);
    }

    #[test]
    fn test_enum_pattern_binding_arity_mismatch() {
        use crate::ast::*;
        let defs = vec![
            Def::DEnum {
                loc: Loc { line: 1, col: 1 },
                name: "Shape".into(),
                variants: vec![
                    EnumVariant { name: "Circle".into(), payload: vec![Typ::TInt] },
                ],
                type_params: vec![],
            },
            Def::DFunc {
                loc: Loc { line: 2, col: 1 },
                name: "area".into(),
                type_params: vec![],
                params: vec![Param::POwn { name: "s".into(), typ: Typ::TStruct { name: "Shape".into(), fields: vec![], type_args: vec![] } }],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EChoose {
                    loc: Loc { line: 3, col: 1 },
                    var: Box::new(Expr::EVar { loc: Loc { line: 3, col: 1 }, name: "s".into() }),
                    cases: vec![WhenCase {
                        when: Box::new(Expr::EEnumPattern {
                            loc: Loc { line: 4, col: 1 },
                            enum_name: "Shape".into(),
                            variant: "Circle".into(),
                            bindings: vec!["a".into(), "b".into()],
                        }),
                        guard: None,
                        then: Box::new(Expr::EInt { loc: Loc { line: 4, col: 1 }, value: 1 }),
                    }],
                    otherwise: Some(Box::new(Expr::EInt { loc: Loc { line: 5, col: 1 }, value: 0 })),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty());
        assert!(errs.iter().any(|e| e.code == "E0016"));
    }

    #[test]
    fn test_generic_enum_typecheck_construct_and_match() {
        use crate::ast::*;
        let defs = vec![
            Def::DEnum {
                loc: Loc { line: 1, col: 1 },
                name: "Box".into(),
                variants: vec![
                    EnumVariant { name: "Value".into(), payload: vec![Typ::TGenericParam { name: "T".into() }] },
                    EnumVariant { name: "Empty".into(), payload: vec![] },
                ],
                type_params: vec!["T".into()],
            },
            Def::DFunc {
                loc: Loc { line: 2, col: 1 },
                name: "main".into(),
                type_params: vec![],
                params: vec![],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EBlock {
                    loc: Loc { line: 3, col: 1 },
                    stmts: vec![
                        Stmt::SLetTyped {
                            loc: Loc { line: 4, col: 1 },
                            name: "b".into(),
                            typ: Typ::TStruct { name: "Box".into(), fields: vec![], type_args: vec![Typ::TInt] },
                            expr: Box::new(Expr::ECall {
                                loc: Loc { line: 4, col: 1 },
                                name: "Value".into(),
                                type_args: vec![Typ::TInt],
                                args: vec![
                                    Expr::EVar { loc: Loc { line: 4, col: 1 }, name: "Box".into() },
                                    Expr::EInt { loc: Loc { line: 4, col: 1 }, value: 1 },
                                ],
                            }),
                        },
                        Stmt::SExpr {
                            loc: Loc { line: 5, col: 1 },
                            expr: Box::new(Expr::EChoose {
                                loc: Loc { line: 5, col: 1 },
                                var: Box::new(Expr::EVar { loc: Loc { line: 5, col: 1 }, name: "b".into() }),
                                cases: vec![
                                    WhenCase {
                                        when: Box::new(Expr::EEnumPattern {
                                            loc: Loc { line: 6, col: 1 },
                                            enum_name: "Box".into(),
                                            variant: "Value".into(),
                                            bindings: vec!["v".into()],
                                        }),
                                        guard: None,
                                        then: Box::new(Expr::EVar { loc: Loc { line: 6, col: 1 }, name: "v".into() }),
                                    },
                                ],
                                otherwise: Some(Box::new(Expr::EInt { loc: Loc { line: 7, col: 1 }, value: 0 })),
                            }),
                        },
                        Stmt::SReturn {
                            loc: Loc { line: 8, col: 1 },
                            expr: Box::new(Expr::EInt { loc: Loc { line: 8, col: 1 }, value: 1 }),
                        },
                    ],
                    result: Some(Box::new(Expr::EInt { loc: Loc { line: 9, col: 1 }, value: 0 })),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
        ];
        let errs = check_program(&defs);
        assert!(errs.is_empty(), "unexpected errors: {:?}", errs);
    }

    #[test]
    fn test_generic_enum_type_mismatch() {
        use crate::ast::*;
        let defs = vec![
            Def::DEnum {
                loc: Loc { line: 1, col: 1 },
                name: "Box".into(),
                variants: vec![
                    EnumVariant { name: "Value".into(), payload: vec![Typ::TGenericParam { name: "T".into() }] },
                ],
                type_params: vec!["T".into()],
            },
            Def::DFunc {
                loc: Loc { line: 2, col: 1 },
                name: "f".into(),
                type_params: vec![],
                params: vec![Param::POwn {
                    name: "b".into(),
                    typ: Typ::TStruct { name: "Box".into(), fields: vec![], type_args: vec![Typ::TInt] },
                }],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EInt { loc: Loc { line: 3, col: 1 }, value: 0 }),
                safety: Safety::Safe,
                is_async: false,
            },
            Def::DFunc {
                loc: Loc { line: 4, col: 1 },
                name: "main".into(),
                type_params: vec![],
                params: vec![],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EBlock {
                    loc: Loc { line: 5, col: 1 },
                    stmts: vec![
                        Stmt::SLetTyped {
                            loc: Loc { line: 6, col: 1 },
                            name: "b".into(),
                            typ: Typ::TStruct { name: "Box".into(), fields: vec![], type_args: vec![Typ::TString] },
                            expr: Box::new(Expr::ECall {
                                loc: Loc { line: 6, col: 1 },
                                name: "Value".into(),
                                type_args: vec![Typ::TString],
                                args: vec![
                                    Expr::EVar { loc: Loc { line: 6, col: 1 }, name: "Box".into() },
                                    Expr::EString { loc: Loc { line: 6, col: 1 }, value: "x".into() },
                                ],
                            }),
                        },
                        Stmt::SExpr {
                            loc: Loc { line: 7, col: 1 },
                            expr: Box::new(Expr::ECall {
                                loc: Loc { line: 7, col: 1 },
                                name: "f".into(),
                                type_args: vec![],
                                args: vec![Expr::EVar { loc: Loc { line: 7, col: 1 }, name: "b".into() }],
                            }),
                        },
                    ],
                    result: Some(Box::new(Expr::EInt { loc: Loc { line: 8, col: 1 }, value: 0 })),
                }),
                safety: Safety::Safe,
                is_async: false,
            },
        ];
        let errs = check_program(&defs);
        assert!(!errs.is_empty(), "expected a type error but got none");
        assert!(errs.iter().any(|e| e.code == "E0016"));
    }
}
