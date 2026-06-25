use crate::ast::*;
use crate::error::Error;
use std::collections::HashMap;

struct TypeEnv {
    vars: HashMap<String, Typ>,
}

fn types_equal(a: &Typ, b: &Typ) -> bool {
    match (a, b) {
        (Typ::TStruct { name: n1, .. }, Typ::TStruct { name: n2, .. }) => n1 == n2,
        (Typ::TArray { of: o1 }, Typ::TArray { of: o2 }) => types_equal(o1, o2),
        (Typ::TPtr { to: t1 }, Typ::TPtr { to: t2 }) => types_equal(t1, t2),
        (Typ::TBox { of: o1 }, Typ::TBox { of: o2 }) => types_equal(o1, o2),
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
        "ptr_alloc" | "ptr_realloc" => Some(Typ::TPtrAny),
        "box_new" | "box_deref" => None,
        _ => None,
    }
}

fn build_func_sigs(defs: &[Def]) -> HashMap<String, (Vec<Param>, Option<Typ>)> {
    let mut sigs = HashMap::new();
    for def in defs {
        match def {
            Def::DFunc {
                name,
                params,
                returns,
                ..
            }
            | Def::DCFuncUnsafe {
                name,
                params,
                returns,
                ..
            } => {
                sigs.insert(name.clone(), (params.clone(), returns.clone()));
            }
            _ => {}
        }
    }
    sigs
}

fn build_struct_map(defs: &[Def]) -> HashMap<String, Vec<FieldDef>> {
    let mut structs = HashMap::new();
    for def in defs {
        if let Def::DStruct { name, fields, .. } = def {
            structs.insert(name.clone(), fields.clone());
        }
    }
    structs
}

fn require_type(
    env: &mut TypeEnv,
    func_sigs: &HashMap<String, (Vec<Param>, Option<Typ>)>,
    structs: &HashMap<String, Vec<FieldDef>>,
    func_return: &Option<Typ>,
    e: &Expr,
) -> (Typ, Vec<Error>) {
    let (typ, errs) = infer_type(env, func_sigs, structs, func_return, e);
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
        | Expr::EFor { loc, .. } => loc.clone(),
    }
}

fn infer_type(
    env: &mut TypeEnv,
    func_sigs: &HashMap<String, (Vec<Param>, Option<Typ>)>,
    structs: &HashMap<String, Vec<FieldDef>>,
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
            let (inner_typ, mut errs) = infer_type(env, func_sigs, structs, func_return, expr);
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
            let (inner_typ, mut errs) = infer_type(env, func_sigs, structs, func_return, expr);
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
            let (lt, mut errs) = require_type(env, func_sigs, structs, func_return, left);
            let (rt, errs2) = require_type(env, func_sigs, structs, func_return, right);
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
            }
        }
        Expr::EIf {
            cond,
            then,
            else_,
            loc,
        } => {
            let (ct, mut errs) = require_type(env, func_sigs, structs, func_return, cond);
            if !types_equal(&ct, &Typ::TBool) {
                errs.push(Error::new(
                    "E0014",
                    loc,
                    &format!("if condition must be bool"),
                ));
            }

            let (tt, errs2) = infer_type(env, func_sigs, structs, func_return, then);
            errs.extend(errs2);

            match else_ {
                Some(else_expr) => {
                    let (et, errs3) = infer_type(env, func_sigs, structs, func_return, else_expr);
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
            let (vt, mut errs) = require_type(env, func_sigs, structs, func_return, var);
            let _ = vt;
            let mut tmp_typ = vec![];

            if let Some(else_expr) = otherwise {
                let (et, errs3) = infer_type(env, func_sigs, structs, func_return, else_expr);
                errs.extend(errs3);

                let mut all_same = true;
                let mut first_type: Option<Typ> = Some(et);
                let mut first_case = true;
                for case in cases {
                    let (wt, errs_w) =
                        require_type(env, func_sigs, structs, func_return, &case.when);
                    errs.extend(errs_w);
                    if !types_equal(&wt, &vt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            "choose expr and the variable must have the same type",
                        ));
                    }
                    let (tt, errs_t) = infer_type(env, func_sigs, structs, func_return, &case.then);
                    errs.extend(errs_t);
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
                    let (wt, errs_w) =
                        require_type(env, func_sigs, structs, func_return, &case.when);
                    errs.extend(errs_w);
                    if !types_equal(&wt, &vt) {
                        errs.push(Error::new(
                            "E0014",
                            loc,
                            "choose expr and the variable must have the same type",
                        ));
                    }
                    let (tt, errs_t) = infer_type(env, func_sigs, structs, func_return, &case.then);
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
        Expr::ECall { name, args, loc } => {
            let mut errs = vec![];
            let mut arg_types = vec![];
            for arg in args {
                let (at, ae) = require_type(env, func_sigs, structs, func_return, arg);
                arg_types.push(at);
                errs.extend(ae);
            }

            let user_func = func_sigs.get(name.as_str());
            let ret_typ = if let Some((params, ret)) = user_func {
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
                ret.clone().unwrap_or(Typ::TNull)
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
                    name => builtin_return_typ(name).unwrap_or(Typ::TNull),
                }
            };
            (ret_typ, errs)
        }
        Expr::EStructLit { name, fields, loc } => {
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
            for vf in fields {
                provided_fields.insert(vf.name.as_str());
                let (ft, fe) = require_type(env, func_sigs, structs, func_return, &vf.value);
                errs.extend(fe);
                match field_map.get(vf.name.as_str()) {
                    Some(expected_t) => {
                        if !types_equal(expected_t, &ft) {
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

            (
                Typ::TStruct {
                    name: name.clone(),
                    fields: struct_fields.clone(),
                },
                errs,
            )
        }
        Expr::EFieldAccess { expr, field, loc } => {
            let (et, mut errs) = require_type(env, func_sigs, structs, func_return, expr);
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
                        errs.push(Error::new(
                            "E0018",
                            loc,
                            &format!("unknown struct '{}'", sname),
                        ));
                        (Typ::TInvalid, errs)
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
            let (first_t, fe) = require_type(env, func_sigs, structs, func_return, &values[0]);
            errs.extend(fe);
            let mut all_same = true;
            for v in &values[1..] {
                let (vt, ve) = require_type(env, func_sigs, structs, func_return, v);
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
            let (et, mut errs) = require_type(env, func_sigs, structs, func_return, expr);
            let valid = match (&et, to) {
                (Typ::TInt, Typ::TFloat32)
                | (Typ::TFloat32, Typ::TInt)
                | (Typ::TInt, Typ::TFloat64)
                | (Typ::TFloat64, Typ::TInt)
                | (Typ::TFloat32, Typ::TFloat64)
                | (Typ::TFloat64, Typ::TFloat32)
                | (Typ::TInt, Typ::TChar)
                | (Typ::TChar, Typ::TInt)
                | (Typ::TBool, Typ::TInt) => true,
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
                        let (t, se) = require_type(env, func_sigs, structs, func_return, expr);
                        errs.extend(se);
                        env.vars.insert(name.clone(), t);
                    }
                    Stmt::SLetTyped { name, typ, expr, loc } => {
                        let (t, se) = require_type(env, func_sigs, structs, func_return, expr);
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
                        let (t, se) = require_type(env, func_sigs, structs, func_return, expr);
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
                    Stmt::SReturn { expr, loc } => {
                        let (t, se) = require_type(env, func_sigs, structs, func_return, expr);
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
                        let (_, se) = infer_type(env, func_sigs, structs, func_return, expr);
                        errs.extend(se);
                    }
                    Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => {}
                }
            }
            if let Some(r) = result {
                let (t, se) = infer_type(env, func_sigs, structs, func_return, r);
                errs.extend(se);
                (t, errs)
            } else {
                (Typ::TNull, errs)
            }
        }
        Expr::EWhile { cond, body, loc } => {
            let mut errs = vec![];
            let (ct, ce) = require_type(env, func_sigs, structs, func_return, cond);
            errs.extend(ce);
            if !types_equal(&ct, &Typ::TBool) {
                errs.push(Error::new(
                    "E0014",
                    loc,
                    &format!("while condition must be bool"),
                ));
            }
            let (_, be) = infer_type(env, func_sigs, structs, func_return, body);
            errs.extend(be);
            (Typ::TNull, errs)
        }
        Expr::ELoop { body, .. } => {
            let mut errs = vec![];
            let (_, be) = infer_type(env, func_sigs, structs, func_return, body);
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
            let (rt, re) = require_type(env, func_sigs, structs, func_return, range);
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
            let (_, be) = infer_type(env, func_sigs, structs, func_return, body);
            errs.extend(be);
            (Typ::TNull, errs)
        }
        Expr::EMacro { .. } => (Typ::TNull, vec![]),
    }
}

pub fn check_program(defs: &[Def]) -> Vec<Error> {
    let func_sigs = build_func_sigs(defs);
    let structs = build_struct_map(defs);
    let mut errs = vec![];

    for def in defs {
        match def {
            Def::DFunc {
                name: _,
                params,
                returns,
                body,
                ..
            } => {
                let mut env = TypeEnv {
                    vars: HashMap::new(),
                };
                for p in params {
                    match p {
                        Param::PRef { name, typ } | Param::POwn { name, typ } => {
                            env.vars.insert(name.clone(), typ.clone());
                        }
                    }
                }
                let (body_t, mut fun_errs) =
                    infer_type(&mut env, &func_sigs, &structs, returns, body);
                errs.append(&mut fun_errs);
                if let Some(ref rt) = returns {
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
                let (_, mut fun_errs) = infer_type(&mut env, &func_sigs, &structs, &None, body);
                errs.append(&mut fun_errs);
            }
            _ => {}
        }
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
            params,
            returns,
            body: Box::new(body),
            safety,
        }
    }

    fn make_struct(name: &str, fields: Vec<FieldDef>) -> Def {
        Def::DStruct {
            loc: loc(),
            name: name.to_string(),
            fields,
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
}
