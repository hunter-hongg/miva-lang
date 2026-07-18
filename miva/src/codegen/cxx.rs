use crate::ast::*;
use crate::symbol_table::SymbolTable;

/// Escape a string value for use in a C++ string literal.
pub fn cxx_escape_string(s: &str) -> String {
    use crate::codegen::resolve_c_escapes;
    let resolved = resolve_c_escapes(s);
    let mut out = String::with_capacity(resolved.len());
    for c in resolved.chars() {
        match c {
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\0' => out.push_str("\\0"),
            '\\' => out.push_str("\\\\"),
            '\'' => out.push_str("\\'"),
            '"' => out.push_str("\\\""),
            c => out.push(c),
        }
    }
    out
}

fn indent_str(n: usize) -> String {
    " ".repeat(n * 2)
}

pub fn cxx_type(typ: &Typ) -> String {
    match typ {
        Typ::TInt => "mvp_builtin_int".into(),
        Typ::TBool => "mvp_builtin_boolean".into(),
        Typ::TFloat64 | Typ::TFloat32 => "mvp_builtin_float".into(),
        Typ::TChar => "mvp_builtin_byte".into(),
        Typ::TString => "mvp_builtin_string".into(),
        Typ::TArray { of } => format!("std::vector<{}>", cxx_type(of)),
        Typ::TStruct {
            name, type_args, ..
        } => {
            if type_args.is_empty() {
                name.clone()
            } else {
                let args_str = type_args
                    .iter()
                    .map(cxx_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, args_str)
            }
        }
        Typ::TPtr { to } => format!("{}*", cxx_type(to)),
        Typ::TBox { of } => format!("mvp_builtin_box<{}>", cxx_type(of)),
        Typ::TFuture { of } => format!("mvp_future<{}>", cxx_type(of)),
        Typ::TNull => "void".into(),
        Typ::TPtrAny => "mvp_builtin_ptrany".into(),
        Typ::TInvalid => "invalid".into(),
        Typ::TGenericParam { name } => name.clone(),
    }
}

fn cxx_param(param: &Param) -> String {
    match param {
        Param::PRef { name, typ } => format!("{}& {}", cxx_type(typ), name),
        Param::POwn { name, typ } => format!("{} {}", cxx_type(typ), name),
    }
}

fn cxx_func_decl(name: &str, params: &[Param], ret: &Option<Typ>) -> String {
    let param_list: Vec<_> = params.iter().map(cxx_param).collect();
    let ret_type = ret.as_ref().map_or("mvp_builtin_unit".into(), cxx_type);
    format!("{} {}({});\n", ret_type, mangle_cpp_kw(name), param_list.join(", "))
}

/// Mangle a Miva identifier that collides with a C++ keyword so the emitted
/// C++ compiles. `new`, `delete`, `class`, `template`, `typename`, etc. get an
/// `mvp_` prefix. Applied to every function/definition name we emit and every
/// call name we reference, so caller and callee stay in sync.
fn mangle_cpp_kw(name: &str) -> String {
    // Only mangle the bare (last) identifier — qualified names like
    // `mvp_std::mem::offset` have namespace separators we must preserve.
    // Split on '::' and mangle only the trailing segment.
    let last_seg_start = name.rfind("::").map(|i| i + 2).unwrap_or(0);
    let prefix = &name[..last_seg_start];
    let tail = &name[last_seg_start..];
    let mangled_tail = match tail {
        "new" | "delete" | "class" | "template" | "typename" | "operator"
        | "public" | "private" | "protected" | "virtual" | "namespace"
        | "using" | "struct" | "enum" | "union" | "typedef" | "auto"
        | "static" | "extern" | "const" | "volatile" | "register" | "inline"
        | "friend" | "this" | "throw" | "try" | "catch" | "goto" | "return"
        | "break" | "continue" | "if" | "else" | "while" | "for" | "switch"
        | "case" | "default" | "do" | "sizeof" | "and" | "or" | "not"
        | "xor" | "bitand" | "bitor" | "compl" | "true" | "false" => {
            format!("mvp_{}", tail)
        }
        _ => tail.to_string(),
    };
    format!("{}{}", prefix, mangled_tail)
}

pub fn map_builtin(name: &str) -> String {
    match name {
        "print" => "mvp_print".into(),
        "prints" => "mvp_prints".into(),
        "println" => "mvp_println".into(),
        "printlns" => "mvp_printlns".into(),
        "error" => "mvp_error".into(),
        "errors" => "mvp_errors".into(),
        "errorln" => "mvp_errorln".into(),
        "errorlns" => "mvp_errorlns".into(),
        "exit" => "mvp_exit".into(),
        "abort" => "mvp_abort".into(),
        "panic" => "mvp_panic".into(),
        "string_concat" => "mvp_string_concat".into(),
        "string_parse" => "mvp_string_parse".into(),
        "string_length" => "mvp_string_length".into(),
        "string_make" => "mvp_string_make".into(),
        "string_from" => "mvp_to_string".into(),
        "box_new" => "mvp_box_new".into(),
        "box_deref" => "mvp_box_deref".into(),
        "range" => "mvp_range".into(),
        "ptr_alloc" => "mvp_alloc".into(),
        "ptr_realloc" => "mvp_realloc".into(),
        "ptr_free" => "mvp_free".into(),
        "ptr_set" => "mvp_builtin_ptrset".into(),
        "ptr_offset" => "mvp_ptr_offset".into(),
        "await" => "mvp_async_await".into(),
        "json_parse" => "mvp_json_parse".into(),
        "json_kind" => "mvp_json_kind".into(),
        "json_bool" => "mvp_json_bool".into(),
        "json_number" => "mvp_json_number".into(),
        "json_string" => "mvp_json_string".into(),
        "json_array_len" => "mvp_json_array_len".into(),
        "json_array_get" => "mvp_json_array_get".into(),
        "json_object_len" => "mvp_json_object_len".into(),
        "json_object_key" => "mvp_json_object_key".into(),
        "json_object_get" => "mvp_json_object_get".into(),
        "json_object_find" => "mvp_json_object_find".into(),
        "json_free" => "mvp_json_free".into(),
        "json_stringify" => "mvp_json_stringify".into(),
        "xml_parse" => "mvp_xml_parse".into(),
        "xml_kind" => "mvp_xml_kind".into(),
        "xml_tag" => "mvp_xml_tag".into(),
        "xml_attr_count" => "mvp_xml_attr_count".into(),
        "xml_attr_name" => "mvp_xml_attr_name".into(),
        "xml_attr_value" => "mvp_xml_attr_value".into(),
        "xml_attr_find" => "mvp_xml_attr_find".into(),
        "xml_child_count" => "mvp_xml_child_count".into(),
        "xml_child_get" => "mvp_xml_child_get".into(),
        "xml_text" => "mvp_xml_text".into(),
        "xml_comment" => "mvp_xml_comment".into(),
        "xml_cdata" => "mvp_xml_cdata".into(),
        "xml_pi_target" => "mvp_xml_pi_target".into(),
        "xml_pi_data" => "mvp_xml_pi_data".into(),
        "xml_stringify" => "mvp_xml_stringify".into(),
        "xml_free" => "mvp_xml_free".into(),
        "toml_parse" => "mvp_toml_parse".into(),
        "toml_kind" => "mvp_toml_kind".into(),
        "toml_bool" => "mvp_toml_bool".into(),
        "toml_number" => "mvp_toml_number".into(),
        "toml_string" => "mvp_toml_string".into(),
        "toml_array_len" => "mvp_toml_array_len".into(),
        "toml_array_get" => "mvp_toml_array_get".into(),
        "toml_object_len" => "mvp_toml_object_len".into(),
        "toml_object_key" => "mvp_toml_object_key".into(),
        "toml_object_get" => "mvp_toml_object_get".into(),
        "toml_object_find" => "mvp_toml_object_find".into(),
        "toml_free" => "mvp_toml_free".into(),
        "toml_stringify" => "mvp_toml_stringify".into(),
        "yaml_parse" => "mvp_yaml_parse".into(),
        "yaml_kind" => "mvp_yaml_kind".into(),
        "yaml_bool" => "mvp_yaml_bool".into(),
        "yaml_number" => "mvp_yaml_number".into(),
        "yaml_string" => "mvp_yaml_string".into(),
        "yaml_array_len" => "mvp_yaml_array_len".into(),
        "yaml_array_get" => "mvp_yaml_array_get".into(),
        "yaml_object_len" => "mvp_yaml_object_len".into(),
        "yaml_object_key" => "mvp_yaml_object_key".into(),
        "yaml_object_get" => "mvp_yaml_object_get".into(),
        "yaml_object_find" => "mvp_yaml_object_find".into(),
        "yaml_free" => "mvp_yaml_free".into(),
        "yaml_stringify" => "mvp_yaml_stringify".into(),
        _ => {
            let parts: Vec<&str> = name.split('.').collect();
            if parts.first() == Some(&"ffi") {
                parts[1..].join("::")
            } else {
                // qualified name like `std.vec.new` → `mvp_std::vec::mvp_new`;
                // mangle only the trailing identifier so C++ keywords (new,
                // delete, class, ...) don't collide at the call site.
                let joined = parts.join("::");
                mangle_cpp_kw(&joined)
            }
        }
    }
}

fn cxx_module(name: &str) -> String {
    module_parts(name).join("::")
}

fn module_parts(name: &str) -> Vec<String> {
    let parts: Vec<&str> = name.split('.').collect();
    if parts.first() == Some(&"std") {
        let mut result = vec!["mvp_std".into()];
        result.extend(parts[1..].iter().map(|s| s.to_string()));
        result
    } else if parts.first() == Some(&"main") {
        let mut result = vec!["mvp_main".into()];
        result.extend(parts[1..].iter().map(|s| s.to_string()));
        result
    } else {
        parts.iter().map(|s| s.to_string()).collect()
    }
}

struct BlockFlow {
    stmts: Vec<String>,
    ret_line: String,
}

fn analyze_block(body: &Expr, depth: usize, ret_type: &str) -> Option<BlockFlow> {
    match body {
        Expr::EBlock { stmts, result, .. } => {
            let inner = depth + 1;
            if ret_type == "mvp_builtin_unit" {
                let stmt_strs: Vec<_> = stmts.iter().map(|s| cxx_stmt(inner, s)).collect();

                let ret = match result {
                    Some(expr) => {
                        format!("{}return {};\n", indent_str(inner), cxx_expr(expr, inner))
                    }
                    None => format!("{}return mvp_builtin_void;\n", indent_str(inner)),
                };
                Some(BlockFlow {
                    stmts: stmt_strs,
                    ret_line: ret,
                })
            } else {
                let (stmts_out, last_expr) = take_last_expr(stmts, inner);
                let ret = match result {
                    Some(expr) => {
                        format!("{}return {};\n", indent_str(inner), cxx_expr(expr, inner))
                    }
                    None => match last_expr {
                        Some(expr) => {
                            format!("{}return {};\n", indent_str(inner), cxx_expr(&expr, inner))
                        }
                        None => String::new(),
                    },
                };
                Some(BlockFlow {
                    stmts: stmts_out,
                    ret_line: ret,
                })
            }
        }
        _ => None,
    }
}

fn take_last_expr(stmts: &[Stmt], depth: usize) -> (Vec<String>, Option<Expr>) {
    let mut rev = stmts.to_vec();
    let last_is_expr = rev
        .last()
        .map(|s| matches!(s, Stmt::SExpr { .. }))
        .unwrap_or(false);
    if last_is_expr {
        if let Some(Stmt::SExpr { expr, .. }) = rev.pop() {
            let out: Vec<_> = rev.iter().map(|s| cxx_stmt(depth, s)).collect();
            return (out, Some(*expr));
        }
    }
    (stmts.iter().map(|s| cxx_stmt(depth, s)).collect(), None)
}

fn cxx_expr(expr: &Expr, depth: usize) -> String {
    match expr {
        Expr::EInt { value, .. } => format!("static_cast<mvp_builtin_int>({})", value),
        Expr::EBool { value, .. } => format!(
            "mvp_builtin_boolean({})",
            if *value { "true" } else { "false" }
        ),
        Expr::EFloat { value, .. } => format!("mvp_builtin_float({})", value.to_string()),
        Expr::EChar { value, .. } => format!("mvp_builtin_byte('{}')", cxx_escape_string(value)),
        Expr::EString { value, .. } => {
            format!("mvp_builtin_string(\"{}\")", cxx_escape_string(value))
        }
        Expr::EVar { name, .. } => name.clone(),
        Expr::EMove { name, .. } => format!("std::move({})", name),
        Expr::EClone { name, .. } => format!("decltype({})({})", name, name),

        Expr::EStructLit {
            name,
            fields,
            type_args,
            ..
        } => cxx_struct_lit(name, type_args, fields, depth),
        Expr::EFieldAccess { expr, field, .. } => {
            if field.chars().all(|c| c.is_ascii_digit()) {
                format!("{}.__payload.field{}", cxx_expr(expr, depth), field)
            } else if let Expr::EVar { name: enum_name, .. } = expr.as_ref() {
                if enum_name.chars().next().map_or(false, |c| c.is_uppercase()) {
                    // Enum discriminant: `Shape.Circle` in a `when (Shape.Circle)`
                    // pattern evaluates to a unit enum value (tag-only) via the
                    // generated `Shape_Circle()` constructor. Enum type names start
                    // uppercase in Miva, so `p.x` (lowercase var) stays `p.x`.
                    format!("{}_{}()", enum_name, field)
                } else {
                    format!("{}.{}", cxx_expr(expr, depth), field)
                }
            } else {
                format!("{}.{}", cxx_expr(expr, depth), field)
            }
        }
        Expr::EBinOp {
            op, left, right, ..
        } => cxx_binop(op, left, right, depth),
        Expr::EIf {
            cond, then, else_, ..
        } => cxx_if(cond, then, else_, depth),
        Expr::EWhile { cond, body, .. } => cxx_while(cond, body, depth),
        Expr::ELoop { body, .. } => cxx_loop(body, depth),
        Expr::EFor {
            var, range, body, ..
        } => cxx_for(var, range, body, depth),
        Expr::ECall {
            name,
            type_args,
            args,
            ..
        } => cxx_call(name, args, type_args, depth),
        Expr::ECast { expr, to, .. } => {
            // Integer <-> pointer reinterpret needs reinterpret_cast (static_cast
            // between int and void* is not legal C++). All other casts go
            // through static_cast as before.
            let from_int = matches!(expr.as_ref(), Expr::EInt { .. })
                || matches!(expr.as_ref(), Expr::EVar { .. });
            let is_ptr_cast = matches!(to, Typ::TPtrAny) || from_int && matches!(to, Typ::TPtrAny);
            if is_ptr_cast {
                format!("reinterpret_cast<{}>({})", cxx_type(to), cxx_expr(expr, depth))
            } else {
                format!("static_cast<{}>({})", cxx_type(to), cxx_expr(expr, depth))
            }
        }
        Expr::EBlock { stmts, result, .. } => cxx_block(stmts, result, depth),
        Expr::EChoose {
            var,
            cases,
            otherwise,
            ..
        } => cxx_choose(var, cases, otherwise, depth),
        Expr::EArrayLit { values, .. } => cxx_array_lit(values, depth),
        Expr::EVoid { .. } => "mvp_builtin_void".into(),
        Expr::EAddr { expr, .. } => format!("&({})", cxx_expr(expr, depth)),
        Expr::EDeref { expr, .. } => format!("*({})", cxx_expr(expr, depth)),
        Expr::EMacro { .. } => String::new(),
        Expr::EMacroVar { .. } => unreachable!(),
        Expr::EMethodCall { .. } => unreachable!(),
    }
}

fn cxx_binop(op: &BinOp, left: &Expr, right: &Expr, depth: usize) -> String {
    let op_str = match op {
        BinOp::Add => " + ",
        BinOp::Sub => " - ",
        BinOp::Mul => " * ",
        BinOp::Eq => " == ",
        BinOp::Neq => " != ",
        BinOp::Lt => " < ",
        BinOp::Gt => " > ",
        BinOp::Le => " <= ",
        BinOp::Ge => " >= ",
        // Short-circuit logical: C++ `&&`/`||` already short-circuit on bool.
        BinOp::And => " && ",
        BinOp::Or => " || ",
    };
    format!(
        "({}{}{})",
        cxx_expr(left, depth),
        op_str,
        cxx_expr(right, depth)
    )
}

fn cxx_call(name: &str, args: &[Expr], type_args: &[Typ], depth: usize) -> String {
    let args_strs: Vec<_> = args.iter().map(|a| cxx_expr(a, depth)).collect();
    if let Some(dot) = name.find('.') {
        let enum_name = &name[..dot];
        let variant = &name[dot + 1..];
        return format!("{}_{}({})", enum_name, variant, args_strs.join(", "));
    } else if let Some(enum_name) = args.first().and_then(|a| match a {
        Expr::EVar { name: n, .. } => Some(n.as_str()),
        _ => None,
    }) {
        // Desugared method-call enum constructor: `Circle(Shape, 5)`
        // (from `Shape.Circle(5)`) -> `Shape_Circle(5)`.
        // Restrict by uppercase: enum type names start uppercase in Miva
        // (e.g. `Shape`, `Color`), while variable names (e.g. `circle`)
        // are lowercase — so `area(circle)` never matches here.
        if enum_name.starts_with(|c: char| c.is_uppercase()) {
            let payload_strs = &args_strs[1..];
            return format!("{}_{}({})", enum_name, name, payload_strs.join(", "));
        }
    }
    let type_arg_str = if type_args.is_empty() {
        String::new()
    } else {
        let tas: Vec<_> = type_args.iter().map(cxx_type).collect();
        format!("<{}>", tas.join(", "))
    };
    format!(
        "{}{}({})",
        map_builtin(name),
        type_arg_str,
        args_strs.join(", ")
    )
}

fn cxx_if(cond: &Expr, then: &Expr, else_: &Option<Box<Expr>>, depth: usize) -> String {
    let cond_str = cxx_expr(cond, depth);
    let then_str = cxx_expr(then, depth + 1);
    let else_str = match else_ {
        Some(e) => format!(" else {{ {}; }}", cxx_expr(e, depth + 1)),
        None => String::new(),
    };
    // Lambda returns void explicitly: when the if-condition is false and
    // there's no else, control falls through to the lambda's closing brace
    // and returns void — no `return` expression for g++ to infer a non-void
    // return type from, so no `ud2`/[[noreturn]] trap on the fall-through
    // path. The then/else bodies are emitted as expression statements
    // (`{ body; }`) rather than `return body;` so a nested lambda inside
    // the body (e.g. an inner if) doesn't make g++ infer this lambda's
    // return type as the inner lambda's.
    format!(
        "([&]() -> void {{ if ({}) {{ {}; }}{} }})()",
        cond_str, then_str, else_str
    )
}

fn cxx_while(cond: &Expr, body: &Expr, depth: usize) -> String {
    let cond_str = cxx_expr(cond, depth);
    let body_str = cxx_expr(body, depth + 1);
    format!("([&]() {{ while ({}) {{ {} ;}}}})()", cond_str, body_str)
}

fn cxx_loop(body: &Expr, depth: usize) -> String {
    let body_str = cxx_expr(body, depth + 1);
    format!("([&]() {{ for (;;) {{ {} ;}}}})()", body_str)
}

fn cxx_for(var: &str, range: &Expr, body: &Expr, depth: usize) -> String {
    let range_str = cxx_expr(range, depth);
    let body_str = cxx_expr(body, depth + 1);
    format!(
        "([&]() {{ for (const auto& {} : {}) {{ {} ;}}}})()",
        var, range_str, body_str
    )
}

fn cxx_struct_lit(name: &str, type_args: &[Typ], fields: &[ValueField], depth: usize) -> String {
    let type_name = if type_args.is_empty() {
        name.to_string()
    } else {
        let args_str = type_args
            .iter()
            .map(cxx_type)
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}<{}>", name, args_str)
    };
    if fields.is_empty() {
        format!("{}{{}}", type_name)
    } else {
        let temp = "__temp";
        let inits: Vec<_> = fields
            .iter()
            .map(|f| format!("{}.{} = {}", temp, f.name, cxx_expr(&f.value, depth + 1)))
            .collect();
        format!(
            "([&]() {{ {} {}={{}}; {}; return {}; }}())",
            type_name,
            temp,
            inits.join("; "),
            temp
        )
    }
}

fn cxx_choose(
    var: &Expr,
    cases: &[WhenCase],
    otherwise: &Option<Box<Expr>>,
    depth: usize,
) -> String {
    let var_str = cxx_expr(var, depth);
    let ind = indent_str(depth);
    let inner = depth + 1;
    let cases_str: String = cases.iter().fold(String::new(), |acc, c| {
        let value_str = cxx_expr(&c.when, depth);
        let body_str = cxx_expr(&c.then, inner);
        format!(
            "{}{}if ({} == {}) {{ return {}; }}\n",
            acc, ind, var_str, value_str, body_str
        )
    });
    let otherwise_str = match otherwise {
        Some(e) => format!("{}else {{ return {}; }}", ind, cxx_expr(e, inner)),
        None => String::new(),
    };
    format!("([&]() {{\n{}{}\n{}\n}}())", cases_str, otherwise_str, ind)
}

fn cxx_block(stmts: &[Stmt], result: &Option<Box<Expr>>, depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    let stmt_strs: String = stmts.iter().fold(String::new(), |acc, stmt| match stmt {
        Stmt::SLet {
            mutable,
            name,
            expr,
            ..
        } => {
            let expr_str = cxx_expr(expr, inner);
            let mut_str = if *mutable { "auto " } else { "const auto " };
            format!(
                "{}{}{}{} = {};\n",
                acc,
                indent_str(inner),
                mut_str,
                name,
                expr_str
            )
        }
        Stmt::SLetTyped {
            name, typ, expr, ..
        } => {
            let expr_str = cxx_expr(expr, inner);
            format!(
                "{}{}{} {} = {};\n",
                acc,
                indent_str(inner),
                cxx_type(typ),
                name,
                expr_str
            )
        }
        _ => format!("{}{}", acc, cxx_stmt(inner, stmt)),
    });
    let result_str = match result {
        Some(expr) => format!("{}return {};\n", indent_str(inner), cxx_expr(expr, inner)),
        None => String::new(),
    };
    format!("([&]() {{\n{}{}{}}})()", stmt_strs, result_str, ind)
}

fn cxx_array_lit(values: &[Expr], depth: usize) -> String {
    let elems: Vec<_> = values.iter().map(|e| cxx_expr(e, depth)).collect();
    format!("std::vector{{{}}}", elems.join(", "))
}

fn cxx_stmt(depth: usize, stmt: &Stmt) -> String {
    let ind = indent_str(depth);
    match stmt {
        Stmt::SLet {
            mutable,
            name,
            expr,
            ..
        } => {
            let mut_str = if *mutable { "auto " } else { "const auto " };
            format!("{}{}{} = {};\n", ind, mut_str, name, cxx_expr(expr, depth))
        }
        Stmt::SLetTyped {
            name, typ, expr, ..
        } => {
            format!(
                "{}{} {} = {};\n",
                ind,
                cxx_type(typ),
                name,
                cxx_expr(expr, depth)
            )
        }
        Stmt::SReturn { expr, .. } => {
            format!("{}return {};\n", ind, cxx_expr(expr, depth))
        }
        Stmt::SExpr { expr, .. } => {
            format!("{}{};\n", ind, cxx_expr(expr, depth))
        }
        Stmt::SAssign { name, expr, .. } => {
            format!("{}{} = {};\n", ind, name, cxx_expr(expr, depth))
        }
        Stmt::SFieldAssign { target, field, expr, .. } => {
            // `target.field = expr` → `(target).field = (expr);` in C++.
            // The parens around target protect chained field writes like
            // `a.b.c = x` from mis-grouping at the C++ level.
            format!(
                "{}({}).{} = ({});\n",
                ind,
                cxx_expr(target, depth),
                field,
                cxx_expr(expr, depth)
            )
        }
        Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => String::new(),
    }
}

fn cxx_def(def: &Def, depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    match def {
        Def::DStruct {
            name,
            fields,
            type_params,
            ..
        } => cxx_struct_def(name, type_params, fields, ind, inner),
        Def::DEnum {
            name,
            variants,
            type_params,
            ..
        } => cxx_enum_def(name, type_params, variants, ind, inner),
        Def::DFunc {
            name,
            type_params,
            params,
            returns,
            body,
            is_async,
            ..
        } if *is_async => cxx_async_func(name, type_params, params, returns, body, ind, inner),
        Def::DFunc {
            name,
            type_params,
            params,
            returns,
            body,
            ..
        } if name == "main" => cxx_main_func(ind, inner, body),
        Def::DCFuncUnsafe {
            name,
            params,
            returns,
            code,
            ..
        } => cxx_cfunc(name, params, returns, code, ind),
        Def::DFunc {
            name,
            type_params,
            params,
            returns,
            body,
            ..
        } => cxx_normal_func(name, type_params, params, returns, body, ind, inner),
        Def::DImpl {
            struct_name, impls, ..
        } => cxx_impl(struct_name, impls, ind),
        Def::DMacro { .. } => unreachable!(),
        Def::DModule { .. }
        | Def::SExport { .. }
        | Def::SImport { .. }
        | Def::SImportAs { .. }
        | Def::SImportHere { .. }
        | Def::DTest { .. }
        | Def::DCMagical { .. }
        | Def::DCIntro { .. } => String::new(),
    }
}

fn cxx_struct_def(
    name: &str,
    type_params: &[String],
    fields: &[FieldDef],
    ind: String,
    inner: usize,
) -> String {
    let field_strs: String = fields
        .iter()
        .map(|f| format!("{}{} {};\n", indent_str(inner), cxx_type(&f.typ), f.name))
        .collect();
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
    format!(
        "{}struct {} {{\n{}{}}};\n\n",
        template_header, name, field_strs, ind
    )
}

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
            "{}inline {} {}_{}({}) {{\n{} {} v;\n{} v.__tag = {};\n{} {};\n{} return v;\n{}}}\n\n",
            ind,
            name,
            name,
            v.name,
            params.join(", "),
            indent_str(inner),
            name,
            indent_str(inner),
            idx,
            indent_str(inner),
            inits.join(&format!("\n{}", indent_str(inner))),
            indent_str(inner),
            ind
        );
        ctors.push_str(&ctor);
    }
    // Unit constructors for enum discriminants: `Shape_Circle()` returns a
    // `Shape` value with the variant's tag (payload zero-initialized), used
    // in `when (Shape.Circle)` pattern matching. Skip 0-field variants,
    // which already have a no-arg payload constructor with the same name.
    for (idx, v) in variants.iter().enumerate() {
        if v.payload.is_empty() {
            continue;
        }
        let disc = format!(
            "{}inline {} {}_{}() {{\n{} {} v;\n{} v.__tag = {};\n{} return v;\n{}}}\n\n",
            ind, name, name, v.name, indent_str(inner), name, indent_str(inner), idx, indent_str(inner), ind
        );
        ctors.push_str(&disc);
    }
    struct_str + &ctors
}

fn cxx_main_func(ind: String, inner: usize, body: &Expr) -> String {
    let signature = "mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)";
    match body {
        Expr::EBlock { stmts, .. } => {
            let stmt_strs: String = stmts
                .iter()
                .map(|s| cxx_stmt(inner, s))
                .collect::<Vec<_>>()
                .join("");
            let mut out = String::new();
            out.push_str(&ind);
            out.push_str(signature);
            out.push_str(" {\n");
            out.push_str(&stmt_strs);
            out.push_str(&indent_str(inner));
            out.push_str("return mvp_builtin_void;\n");
            out.push_str(&ind);
            out.push_str("}\n\n");
            out
        }
        _ => format!(
            "{} {} {{ {}; return mvp_builtin_void; }}\n\n",
            ind,
            signature,
            cxx_expr(body, 0)
        ),
    }
}

fn cxx_cfunc(
    name: &str,
    params: &[Param],
    returns: &Option<Typ>,
    code: &str,
    ind: String,
) -> String {
    let param_strs: Vec<_> = params.iter().map(cxx_param).collect();
    let ret_type = returns.as_ref().map_or("mvp_builtin_unit".into(), cxx_type);
    let signature = format!("{} {}({})", ret_type, mangle_cpp_kw(name), param_strs.join(", "));
    format!("{} {} {{\n{}{}}}\n\n", ind, signature, code, ind)
}

fn cxx_normal_func(
    name: &str,
    type_params: &[String],
    params: &[Param],
    returns: &Option<Typ>,
    body: &Expr,
    ind: String,
    _inner: usize,
) -> String {
    let param_strs: Vec<_> = params.iter().map(cxx_param).collect();
    let ret_type = returns.as_ref().map_or("mvp_builtin_unit".into(), cxx_type);
    let signature = format!("{} {}({})", ret_type, mangle_cpp_kw(name), param_strs.join(", "));
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

    let body_str = match body {
        Expr::EBlock { .. } => {
            let flow = analyze_block(body, 0, &ret_type).unwrap_or(BlockFlow {
                stmts: vec![],
                ret_line: "".to_string(),
            });
            format!(
                "{} {} {{\n{}{}{}}}\n\n",
                ind,
                signature,
                flow.stmts.join(""),
                flow.ret_line,
                ind
            )
        }
        _ => {
            if ret_type == "mvp_builtin_unit" {
                format!(
                    "{} {} {{ {}; return mvp_builtin_void; }}\n\n",
                    ind,
                    signature,
                    cxx_expr(body, 0)
                )
            } else {
                format!(
                    "{} {} {{ return {}; }}\n\n",
                    ind,
                    signature,
                    cxx_expr(body, 0)
                )
            }
        }
    };
    format!("{}{}", template_header, body_str)
}

/// Generate the inner statements + return line of a function body (without the
/// surrounding braces), for the given C++ return type.
fn cxx_func_inner(ret_type: &str, body: &Expr, _depth: usize) -> String {
    match body {
        Expr::EBlock { .. } => {
            let flow = analyze_block(body, 0, ret_type).unwrap_or(BlockFlow {
                stmts: vec![],
                ret_line: String::new(),
            });
            format!("{}{}", flow.stmts.join(""), flow.ret_line)
        }
        _ => {
            if ret_type == "mvp_builtin_unit" {
                format!("{}; return mvp_builtin_void;\n", cxx_expr(body, 0))
            } else {
                format!("return {};\n", cxx_expr(body, 0))
            }
        }
    }
}

/// Generate an `async` function. The body returns the inner type T; the
/// function spawns a real OS thread running the body and returns a
/// `future[T]` handle. `.await()` joins the thread and yields the result.
fn cxx_async_func(
    name: &str,
    type_params: &[String],
    params: &[Param],
    returns: &Option<Typ>,
    body: &Expr,
    ind: String,
    inner: usize,
) -> String {
    let ret_type = returns
        .as_ref()
        .map_or_else(|| "mvp_future<mvp_builtin_unit>".to_string(), cxx_type);
    let inner_typ = match returns {
        Some(Typ::TFuture { of }) => (**of).clone(),
        _ => Typ::TNull,
    };
    let inner_ret = cxx_type(&inner_typ);
    let param_strs: Vec<_> = params.iter().map(cxx_param).collect();
    let signature = format!("{} {}({})", ret_type, mangle_cpp_kw(name), param_strs.join(", "));
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
    // Capture each parameter by value so the spawned thread owns its inputs
    // (this also copies `ref` parameters by value, avoiding dangling refs).
    let capture_list = if params.is_empty() {
        "[]".to_string()
    } else {
        let names: Vec<_> = params
            .iter()
            .map(|p| match p {
                Param::PRef { name, .. } | Param::POwn { name, .. } => name.clone(),
            })
            .collect();
        format!("[{}]", names.join(", "))
    };
    let inner_body = cxx_func_inner(&inner_ret, body, inner + 1);
    let lambda = format!(
        "return mvp_async_spawn({}() -> {} {{\n{}}});\n",
        capture_list, inner_ret, inner_body
    );
    format!(
        "{}{} {{\n{}{}{}}}\n\n",
        template_header,
        signature,
        indent_str(inner),
        lambda,
        ind
    )
}

fn cxx_impl(struct_name: &str, impls: &[ImplExpr], ind: String) -> String {
    let mut ret = String::new();
    for impl_expr in impls {
        let op = &impl_expr.op;
        let fn_name = &impl_expr.func;
        let ret_typ = match op {
            ImplOp::ImAdd | ImplOp::ImSub | ImplOp::ImMul => struct_name.to_string(),
            ImplOp::ImEq | ImplOp::ImNeq => "mvp_builtin_boolean".to_string(),
        };
        let operator = match op {
            ImplOp::ImAdd => "+",
            ImplOp::ImSub => "-",
            ImplOp::ImMul => "*",
            ImplOp::ImEq => "==",
            ImplOp::ImNeq => "!=",
        };
        ret.push_str(&format!(
            "{} {} operator{}(const {}& ____a, const {}& ____b) {{ return {}(____a, ____b); }}\n\n",
            ind, ret_typ, operator, struct_name, struct_name, fn_name
        ));
    }
    ret
}

fn cxx_include_path(path: &str) -> String {
    if let Some(c_path) = path.strip_prefix("c:") {
        return format!("#include <{}>\n", c_path);
    }
    let parts: Vec<&str> = path.split('/').collect();
    match crate::config::Config::project_name() {
        Some(proj_name) => {
            if path.starts_with(&proj_name) {
                if parts.len() > 1 {
                    format!("#include <src/{}.miva.h>\n", parts[1..].join("/"))
                } else {
                    String::new()
                }
            } else if let Some(head) = parts.first() {
                if parts.len() > 1 {
                    format!("#include <{}/src/{}.miva.h>\n", head, parts[1..].join("/"))
                } else {
                    format!("#include <{}.miva.h>\n", head)
                }
            } else {
                String::new()
            }
        }
        None => String::new(),
    }
}

fn cxx_include_here(path: &str) -> String {
    let include = cxx_include_path(path);
    if path.starts_with("c:") {
        include
    } else {
        let ns = import_path_to_namespace(path);
        if ns.is_empty() {
            include
        } else {
            format!("{}using namespace {};\n", include, ns)
        }
    }
}

fn import_path_to_namespace(path: &str) -> String {
    if path.starts_with("c:") {
        return String::new();
    }
    if let Some(proj_name) = crate::config::Config::project_name() {
        if let Some(remaining) = path.strip_prefix(&format!("{}/", proj_name)) {
            return cxx_module(&remaining.replace('/', "."));
        }
    }
    cxx_module(&path.replace('/', "."))
}

fn generate_test(defs: &[Def]) -> String {
    let modname = defs
        .iter()
        .find_map(|d| match d {
            Def::DModule { name, .. } => Some(name.clone()),
            _ => None,
        })
        .unwrap_or_default();
    let modname_cxx = cxx_module(&modname);
    let header_fixed = "\
#include <mvp_test.h>
#include <mvp_builtin.h>

using namespace std;

";
    let header = format!("{}\nusing namespace {};\n", header_fixed, modname_cxx);

    let mut body = String::new();
    let mut test_names: Vec<String> = Vec::new();

    for def in defs {
        if let Def::DTest {
            name, body: expr, ..
        } = def
        {
            test_names.push(name.clone());
            let signature = format!("mvp_builtin_int {}()", name);

            let body_str = match expr.as_ref() {
                Expr::EBlock { .. } => {
                    let flow = analyze_block(expr, 0, "mvp_builtin_int").unwrap_or(BlockFlow {
                        stmts: vec![],
                        ret_line: String::new(),
                    });
                    format!(
                        "{} {{\n{}{}{}}}\n\n",
                        signature,
                        flow.stmts.join(""),
                        flow.ret_line,
                        indent_str(0)
                    )
                }
                _ => format!("{} {{ return {}; }}\n\n", signature, cxx_expr(expr, 0)),
            };
            body.push_str(&body_str);
        }
    }

    if test_names.is_empty() {
        return String::new();
    }

    let test_array: String = test_names
        .iter()
        .map(|n| format!("    {{\"{}\", {}}},\n", n, n))
        .collect();

    format!(
        "{}{}\nint main() {{\n    MvpTest tests[] = {{\n{}}};\n    return mvp_run_tests(tests, sizeof(tests) / sizeof(tests[0]));\n}}\n",
        header, body, test_array
    )
}

fn generate_header(defs: &[Def]) -> String {
    let sym = SymbolTable::build(defs);
    let mut exported = String::new();
    collect_exported_rec(defs, &sym, &[], &mut exported);
    if exported.is_empty() {
        return String::new();
    }
    // Emit #include for every imported module header so cross-module symbols
    // (e.g. `mvp_std::mem::alloc` used by vec.miva's template bodies) are
    // declared at the point of instantiation. C++ templates require the
    // callee's declaration be visible in the header that holds the template
    // body, so this is necessary not just nice-to-have.
    let mut includes = String::from("#include <mvp_builtin.h>\n");
    for d in defs.iter() {
        let path = match d {
            Def::SImport { path, .. }
            | Def::SImportHere { path, .. }
            | Def::SImportAs { path, .. } => path,
            _ => continue,
        };
        let inc = cxx_include_path(path);
        if !inc.is_empty() {
            includes.push_str(&inc);
        }
    }
    format!("#pragma once\n\n{}\n{}\n", includes, exported)
}

fn collect_exported_rec(
    defs: &[Def],
    sym: &SymbolTable,
    current_modules: &[String],
    result: &mut String,
) {
    for def in defs.iter() {
        match def {
            Def::DModule { name, .. } => {
                let mut new_modules = current_modules.to_vec();
                new_modules.push(name.clone());
                let parts = module_parts(name);
                let ns_start: String = parts
                    .iter()
                    .map(|p| format!("namespace {} {{\n\n", p))
                    .collect();
                let ns_end: String = parts.iter().map(|_| "}\n\n".to_string()).collect();
                result.push_str(&ns_start);
                collect_exported_rec(&defs[1..], sym, &new_modules, result);
                result.push_str(&ns_end);
                return;
            }
            Def::SExport { symbol, .. } => {
                let decl = if let Some(s) = sym.lookup_struct(symbol) {
                    if let Some(dfunc) = find_struct_def(defs, symbol) {
                        dfunc
                    } else {
                        let field_strs: String = s
                            .fields
                            .iter()
                            .map(|f| format!("  {} {};\n", cxx_type(&f.typ), f.name))
                            .collect();
                        format!("struct {} {{\n{}}};\n\n", s.name, field_strs)
                    }
                } else if let Some(f) = sym.lookup_function(symbol) {
                    if f.type_params.is_empty() {
                        cxx_func_decl(&f.name, &f.params, &f.return_typ)
                    } else {
                        find_func_def(defs, symbol)
                            .map(|dfunc| match dfunc {
                                Def::DFunc {
                                    name,
                                    type_params,
                                    params,
                                    returns,
                                    body,
                                    ..
                                } => cxx_normal_func(
                                    name,
                                    type_params,
                                    params,
                                    returns,
                                    body,
                                    String::new(),
                                    1,
                                ),
                                _ => {
                                    eprintln!("Warning: unexpected def type for exported generic function '{}'", symbol);
                                    String::new()
                                }
                            })
                            .unwrap_or_else(|| {
                                eprintln!("Warning: could not find definition for exported generic function '{}'", symbol);
                                String::new()
                            })
                    }
                } else {
                    String::new()
                };
                result.push_str(&decl);
            }
            _ => {}
        }
    }
}

fn find_func_def<'a>(defs: &'a [Def], name: &str) -> Option<&'a Def> {
    for def in defs.iter() {
        match def {
            Def::DFunc { name: n, .. } if n == name => return Some(def),
            Def::DModule { .. } => {
                if let Some(found) = find_func_def(&defs[1..], name) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

fn find_struct_def(defs: &[Def], name: &str) -> Option<String> {
    for def in defs.iter() {
        match def {
            Def::DStruct {
                name: n,
                fields,
                type_params,
                ..
            } if n == name => {
                return Some(cxx_struct_def(name, type_params, fields, String::new(), 1));
            }
            Def::DModule { .. } => {
                if let Some(found) = find_struct_def(&defs[1..], name) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

struct ScopeParts {
    includes: String,
    defs_str: String,
    main_functions: String,
}

fn generate_with_scope(defs: &[Def], module: Option<&str>) -> ScopeParts {
    let mut includes = String::new();
    let mut defs_str = String::new();
    let mut main_functions = String::new();

    for (i, def) in defs.iter().enumerate() {
        match def {
            Def::DModule { name, .. } => {
                let parts = module_parts(name);
                let ns_start: String = parts
                    .iter()
                    .map(|p| format!("namespace {} {{\n\n", p))
                    .collect();
                let ns_end: String = parts.iter().map(|_| "}\n\n".to_string()).collect();
                let inner = generate_with_scope(&defs[i + 1..], Some(name.as_str()));
                includes.push_str(&inner.includes);
                defs_str.push_str(&ns_start);
                defs_str.push_str(&inner.defs_str);
                defs_str.push_str(&ns_end);
                main_functions.push_str(&inner.main_functions);
                break;
            }
            _ if is_main_func(def) => {
                let mvp_main_str = cxx_def(def, 0);
                defs_str.push_str(&mvp_main_str);
                let global_main = if let Some(name) = module {
                    format!(
                        "int main(int argc, char** argv)\n{{\n  try {{\n  {}::mvp_own_main(argc);\n  }} catch (std::exception& e) {{\n     mvp_errorlns(\"panic: \", e.what());}}\n  return 0;\n}}\n\n",
                        cxx_module(name)
                    )
                } else {
                    "int main(int argc, char** argv)\n{\n  mvp_own_main(argc);\n  return 0;\n}\n\n"
                        .into()
                };
                main_functions.push_str(&global_main);
            }
            Def::SImport { path, .. } => includes.push_str(&cxx_include_here(path)),
            Def::SImportAs { path, .. } => includes.push_str(&cxx_include_path(path)),
            Def::SImportHere { path, .. } => includes.push_str(&cxx_include_here(path)),
            _ => defs_str.push_str(&cxx_def(def, 0)),
        }
    }

    ScopeParts {
        includes,
        defs_str,
        main_functions,
    }
}

fn is_main_func(def: &Def) -> bool {
    matches!(def, Def::DFunc { name, .. } if name == "main")
}

pub fn build_ir(defs: &[Def]) -> [String; 3] {
    let header_content = generate_header(defs);
    let ScopeParts {
        includes,
        defs_str: module_content,
        main_functions,
    } = generate_with_scope(defs, None);
    let preamble = "\
#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <mvp_builtin.h>


using namespace std;

";
    let program = format!(
        "{}{}{}{}\n",
        preamble, includes, module_content, main_functions
    );
    let test = generate_test(defs);
    [program, header_content, test]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Loc {
        Loc { line: 1, col: 1 }
    }

    // ===== Unicode string encoding =====

    #[test]
    fn test_cxx_expr_string_unicode_bytes() {
        let value = "\u{2550}";
        assert_eq!(
            value.as_bytes(),
            &[0xe2, 0x95, 0x90],
            "Rust string should contain correct UTF-8"
        );

        let e = Expr::EString {
            loc: loc(),
            value: value.to_string(),
        };
        let result = cxx_expr(&e, 0);
        let prefix = "mvp_builtin_string(\"";
        let suffix = "\")";
        assert!(
            result.starts_with(prefix),
            "Result should start with prefix"
        );
        assert!(result.ends_with(suffix), "Result should end with suffix");
        let inner = &result[prefix.len()..result.len() - suffix.len()];
        assert_eq!(
            inner.as_bytes(),
            &[0xe2, 0x95, 0x90],
            "Inner string value should be correct UTF-8, not C3 A2 C2 95 C2 90"
        );
    }

    #[test]
    fn test_cxx_enum_def() {
        use crate::ast::*;
        let def = Def::DEnum {
            loc: Loc { line: 1, col: 1 },
            name: "Color".into(),
            variants: vec![
                EnumVariant { name: "Red".into(), payload: vec![] },
                EnumVariant { name: "Green".into(), payload: vec![Typ::TInt] },
            ],
            type_params: vec![],
        };
        let out = cxx_def(&def, 0);
        assert!(out.contains("struct Color"), "expected struct Color in:\n{}", out);
        assert!(out.contains("__tag"), "expected __tag in:\n{}", out);
        assert!(out.contains("Color_Green("), "expected Color_Green ctor in:\n{}", out);
    }

    #[test]
    fn test_build_ir_unicode_with_macro_expansion() {
        let body_expr = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SExpr {
                loc: loc(),
                expr: Box::new(Expr::ECall {
                    loc: loc(),
                    name: "print".to_string(),
                    type_args: vec![],
                    args: vec![Expr::EString {
                        loc: loc(),
                        value: "\u{2550}\u{2550}\u{2550} Hello\u{2550}\u{2550}\u{2550}".to_string(),
                    }],
                }),
            }],
            result: None,
        };

        let defs = vec![Def::DFunc {
            loc: loc(),
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            returns: None,
            body: Box::new(body_expr),
            safety: Safety::Safe,
            is_async: false,
        }];

        let [program, _header, _test] = build_ir(&defs);

        let correct: &[u8] = &[0xe2, 0x95, 0x90];
        let wrong_triple: &[u8] = &[0xc3, 0xa2, 0xc2, 0x95, 0xc2, 0x90];

        assert!(
            program.as_bytes().windows(3).any(|w| w == correct),
            "program bytes should contain correct UTF-8 for \u{2550}"
        );

        assert!(
            !program.as_bytes().windows(6).any(|w| w == wrong_triple),
            "program bytes should NOT contain double-encoded UTF-8"
        );
    }

    // ===== indent_str =====

    #[test]
    fn test_indent_str_zero() {
        assert_eq!(indent_str(0), "");
    }

    #[test]
    fn test_indent_str_one() {
        assert_eq!(indent_str(1), "  ");
    }

    #[test]
    fn test_indent_str_three() {
        assert_eq!(indent_str(3), "      ");
    }

    // ===== cxx_type =====

    #[test]
    fn test_cxx_type_int() {
        assert_eq!(cxx_type(&Typ::TInt), "mvp_builtin_int");
    }

    #[test]
    fn test_cxx_type_bool() {
        assert_eq!(cxx_type(&Typ::TBool), "mvp_builtin_boolean");
    }

    #[test]
    fn test_cxx_type_float64() {
        assert_eq!(cxx_type(&Typ::TFloat64), "mvp_builtin_float");
    }

    #[test]
    fn test_cxx_type_float32() {
        assert_eq!(cxx_type(&Typ::TFloat32), "mvp_builtin_float");
    }

    #[test]
    fn test_cxx_type_char() {
        assert_eq!(cxx_type(&Typ::TChar), "mvp_builtin_byte");
    }

    #[test]
    fn test_cxx_type_string() {
        assert_eq!(cxx_type(&Typ::TString), "mvp_builtin_string");
    }

    #[test]
    fn test_cxx_type_array() {
        let typ = Typ::TArray {
            of: Box::new(Typ::TInt),
        };
        assert_eq!(cxx_type(&typ), "std::vector<mvp_builtin_int>");
    }

    #[test]
    fn test_cxx_type_nested_array() {
        let inner = Typ::TArray {
            of: Box::new(Typ::TInt),
        };
        let outer = Typ::TArray {
            of: Box::new(inner),
        };
        assert_eq!(
            cxx_type(&outer),
            "std::vector<std::vector<mvp_builtin_int>>"
        );
    }

    #[test]
    fn test_cxx_type_struct() {
        let typ = Typ::TStruct {
            name: "Point".into(),
            fields: vec![],
            type_args: vec![],
        };
        assert_eq!(cxx_type(&typ), "Point");
    }

    #[test]
    fn test_cxx_type_ptr() {
        let typ = Typ::TPtr {
            to: Box::new(Typ::TInt),
        };
        assert_eq!(cxx_type(&typ), "mvp_builtin_int*");
    }

    #[test]
    fn test_cxx_type_box() {
        let typ = Typ::TBox {
            of: Box::new(Typ::TInt),
        };
        assert_eq!(cxx_type(&typ), "mvp_builtin_box<mvp_builtin_int>");
    }

    #[test]
    fn test_cxx_type_null() {
        assert_eq!(cxx_type(&Typ::TNull), "void");
    }

    #[test]
    fn test_cxx_type_ptrany() {
        assert_eq!(cxx_type(&Typ::TPtrAny), "mvp_builtin_ptrany");
    }

    #[test]
    fn test_cxx_type_invalid() {
        assert_eq!(cxx_type(&Typ::TInvalid), "invalid");
    }

    // ===== cxx_param =====

    #[test]
    fn test_cxx_param_ref() {
        let p = Param::PRef {
            name: "x".into(),
            typ: Typ::TInt,
        };
        assert_eq!(cxx_param(&p), "mvp_builtin_int const& x");
    }

    #[test]
    fn test_cxx_param_own() {
        let p = Param::POwn {
            name: "flag".into(),
            typ: Typ::TBool,
        };
        assert_eq!(cxx_param(&p), "mvp_builtin_boolean flag");
    }

    #[test]
    fn test_cxx_param_ref_string() {
        let p = Param::PRef {
            name: "s".into(),
            typ: Typ::TString,
        };
        assert_eq!(cxx_param(&p), "mvp_builtin_string const& s");
    }

    // ===== cxx_func_decl =====

    #[test]
    fn test_cxx_func_decl_no_return() {
        let result = cxx_func_decl("foo", &[], &None);
        assert_eq!(result, "mvp_builtin_unit foo();\n");
    }

    #[test]
    fn test_cxx_func_decl_with_return() {
        let result = cxx_func_decl("add", &[], &Some(Typ::TInt));
        assert_eq!(result, "mvp_builtin_int add();\n");
    }

    #[test]
    fn test_cxx_func_decl_with_params() {
        let params = vec![
            Param::POwn {
                name: "a".into(),
                typ: Typ::TInt,
            },
            Param::POwn {
                name: "b".into(),
                typ: Typ::TInt,
            },
        ];
        let result = cxx_func_decl("add", &params, &Some(Typ::TInt));
        assert_eq!(
            result,
            "mvp_builtin_int add(mvp_builtin_int a, mvp_builtin_int b);\n"
        );
    }

    // ===== map_builtin =====

    #[test]
    fn test_map_builtin_print() {
        assert_eq!(map_builtin("print"), "mvp_print");
    }

    #[test]
    fn test_map_builtin_prints() {
        assert_eq!(map_builtin("prints"), "mvp_prints");
    }

    #[test]
    fn test_map_builtin_println() {
        assert_eq!(map_builtin("println"), "mvp_println");
    }

    #[test]
    fn test_map_builtin_printlns() {
        assert_eq!(map_builtin("printlns"), "mvp_printlns");
    }

    #[test]
    fn test_map_builtin_error() {
        assert_eq!(map_builtin("error"), "mvp_error");
    }

    #[test]
    fn test_map_builtin_errors() {
        assert_eq!(map_builtin("errors"), "mvp_errors");
    }

    #[test]
    fn test_map_builtin_errorln() {
        assert_eq!(map_builtin("errorln"), "mvp_errorln");
    }

    #[test]
    fn test_map_builtin_errorlns() {
        assert_eq!(map_builtin("errorlns"), "mvp_errorlns");
    }

    #[test]
    fn test_map_builtin_exit() {
        assert_eq!(map_builtin("exit"), "mvp_exit");
    }

    #[test]
    fn test_map_builtin_abort() {
        assert_eq!(map_builtin("abort"), "mvp_abort");
    }

    #[test]
    fn test_map_builtin_panic() {
        assert_eq!(map_builtin("panic"), "mvp_panic");
    }

    #[test]
    fn test_map_builtin_string_concat() {
        assert_eq!(map_builtin("string_concat"), "mvp_string_concat");
    }

    #[test]
    fn test_map_builtin_string_parse() {
        assert_eq!(map_builtin("string_parse"), "mvp_string_parse");
    }

    #[test]
    fn test_map_builtin_string_length() {
        assert_eq!(map_builtin("string_length"), "mvp_string_length");
    }

    #[test]
    fn test_map_builtin_string_make() {
        assert_eq!(map_builtin("string_make"), "mvp_string_make");
    }

    #[test]
    fn test_map_builtin_string_from() {
        assert_eq!(map_builtin("string_from"), "mvp_to_string");
    }

    #[test]
    fn test_map_builtin_box_new() {
        assert_eq!(map_builtin("box_new"), "mvp_box_new");
    }

    #[test]
    fn test_map_builtin_box_deref() {
        assert_eq!(map_builtin("box_deref"), "mvp_box_deref");
    }

    #[test]
    fn test_map_builtin_range() {
        assert_eq!(map_builtin("range"), "mvp_range");
    }

    #[test]
    fn test_map_builtin_ptr_alloc() {
        assert_eq!(map_builtin("ptr_alloc"), "mvp_alloc");
    }

    #[test]
    fn test_map_builtin_ptr_realloc() {
        assert_eq!(map_builtin("ptr_realloc"), "mvp_realloc");
    }

    #[test]
    fn test_map_builtin_ptr_free() {
        assert_eq!(map_builtin("ptr_free"), "mvp_free");
    }

    #[test]
    fn test_map_builtin_ptr_set() {
        assert_eq!(map_builtin("ptr_set"), "mvp_builtin_ptrset");
    }

    #[test]
    fn test_map_builtin_ptr_offset() {
        assert_eq!(map_builtin("ptr_offset"), "mvp_ptr_offset");
    }

    #[test]
    fn test_map_builtin_ffi() {
        assert_eq!(map_builtin("ffi.some_c_func"), "some_c_func");
    }

    #[test]
    fn test_map_builtin_user_func() {
        assert_eq!(map_builtin("user.func"), "user::func");
    }

    #[test]
    fn test_map_builtin_multi_dot() {
        assert_eq!(map_builtin("a.b.c"), "a::b::c");
    }

    // ===== module_parts =====

    #[test]
    fn test_module_parts_std() {
        assert_eq!(module_parts("std.io"), vec!["mvp_std", "io"]);
    }

    #[test]
    fn test_module_parts_main() {
        assert_eq!(module_parts("main.app"), vec!["mvp_main", "app"]);
    }

    #[test]
    fn test_module_parts_custom() {
        assert_eq!(module_parts("my.module"), vec!["my", "module"]);
    }

    #[test]
    fn test_module_parts_deep() {
        assert_eq!(module_parts("a.b.c"), vec!["a", "b", "c"]);
    }

    #[test]
    fn test_module_parts_single() {
        assert_eq!(module_parts("foo"), vec!["foo"]);
    }

    // ===== cxx_module =====

    #[test]
    fn test_cxx_module_std() {
        assert_eq!(cxx_module("std.io"), "mvp_std::io");
    }

    #[test]
    fn test_cxx_module_main() {
        assert_eq!(cxx_module("main.app"), "mvp_main::app");
    }

    #[test]
    fn test_cxx_module_custom() {
        assert_eq!(cxx_module("my.module"), "my::module");
    }

    #[test]
    fn test_cxx_module_single() {
        assert_eq!(cxx_module("foo"), "foo");
    }

    // ===== cxx_expr - primitives =====

    #[test]
    fn test_cxx_expr_int() {
        let e = Expr::EInt {
            loc: loc(),
            value: 42,
        };
        assert_eq!(cxx_expr(&e, 0), "static_cast<mvp_builtin_int>(42)");
    }

    #[test]
    fn test_cxx_expr_neg_int() {
        let e = Expr::EInt {
            loc: loc(),
            value: -5,
        };
        assert_eq!(cxx_expr(&e, 0), "static_cast<mvp_builtin_int>(-5)");
    }

    #[test]
    fn test_cxx_expr_bool_true() {
        let e = Expr::EBool {
            loc: loc(),
            value: true,
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_boolean(true)");
    }

    #[test]
    fn test_cxx_expr_bool_false() {
        let e = Expr::EBool {
            loc: loc(),
            value: false,
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_boolean(false)");
    }

    #[test]
    fn test_cxx_expr_float() {
        let e = Expr::EFloat {
            loc: loc(),
            value: 3.14,
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_float(3.14)");
    }

    #[test]
    fn test_cxx_expr_float_zero() {
        let e = Expr::EFloat {
            loc: loc(),
            value: 0.0,
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_float(0)");
    }

    #[test]
    fn test_cxx_expr_char() {
        let e = Expr::EChar {
            loc: loc(),
            value: "a".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_byte('a')");
    }

    #[test]
    fn test_cxx_expr_string() {
        let e = Expr::EString {
            loc: loc(),
            value: "hello".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_string(\"hello\")");
    }

    #[test]
    fn test_cxx_expr_var() {
        let e = Expr::EVar {
            loc: loc(),
            name: "x".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "x");
    }

    #[test]
    fn test_cxx_expr_move() {
        let e = Expr::EMove {
            loc: loc(),
            name: "x".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "std::move(x)");
    }

    #[test]
    fn test_cxx_expr_clone() {
        let e = Expr::EClone {
            loc: loc(),
            name: "x".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "decltype(x)(x)");
    }

    #[test]
    fn test_cxx_expr_void() {
        let e = Expr::EVoid { loc: loc() };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_void");
    }

    #[test]
    fn test_cxx_expr_addr() {
        let e = Expr::EAddr {
            loc: loc(),
            expr: Box::new(Expr::EVar {
                loc: loc(),
                name: "x".into(),
            }),
        };
        assert_eq!(cxx_expr(&e, 0), "&(x)");
    }

    #[test]
    fn test_cxx_expr_deref() {
        let e = Expr::EDeref {
            loc: loc(),
            expr: Box::new(Expr::EVar {
                loc: loc(),
                name: "p".into(),
            }),
        };
        assert_eq!(cxx_expr(&e, 0), "*(p)");
    }

    #[test]
    fn test_cxx_expr_macro_empty() {
        let e = Expr::EMacro {
            loc: loc(),
            name: "something".into(),
            args: vec![],
        };
        assert_eq!(cxx_expr(&e, 0), "");
    }

    #[test]
    fn test_cxx_expr_field_access() {
        let e = Expr::EFieldAccess {
            loc: loc(),
            expr: Box::new(Expr::EVar {
                loc: loc(),
                name: "p".into(),
            }),
            field: "x".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "p.x");
    }

    #[test]
    fn test_cxx_expr_cast() {
        let e = Expr::ECast {
            loc: loc(),
            expr: Box::new(Expr::EInt {
                loc: loc(),
                value: 65,
            }),
            to: Typ::TChar,
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "static_cast<mvp_builtin_byte>(static_cast<mvp_builtin_int>(65))"
        );
    }

    // ===== cxx_binop =====

    #[test]
    fn test_cxx_binop_add() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp {
            loc: loc(),
            op: BinOp::Add,
            left: Box::new(left),
            right: Box::new(right),
        };
        assert_eq!(cxx_expr(&e, 0), "(a + b)");
    }

    #[test]
    fn test_cxx_binop_sub() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp {
            loc: loc(),
            op: BinOp::Sub,
            left: Box::new(left),
            right: Box::new(right),
        };
        assert_eq!(cxx_expr(&e, 0), "(a - b)");
    }

    #[test]
    fn test_cxx_binop_mul() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp {
            loc: loc(),
            op: BinOp::Mul,
            left: Box::new(left),
            right: Box::new(right),
        };
        assert_eq!(cxx_expr(&e, 0), "(a * b)");
    }

    #[test]
    fn test_cxx_binop_eq() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp {
            loc: loc(),
            op: BinOp::Eq,
            left: Box::new(left),
            right: Box::new(right),
        };
        assert_eq!(cxx_expr(&e, 0), "(a == b)");
    }

    #[test]
    fn test_cxx_binop_neq() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp {
            loc: loc(),
            op: BinOp::Neq,
            left: Box::new(left),
            right: Box::new(right),
        };
        assert_eq!(cxx_expr(&e, 0), "(a != b)");
    }

    // ===== cxx_call =====

    #[test]
    fn test_cxx_call_no_args() {
        let result = cxx_call("foo", &[], &[], 0);
        assert_eq!(result, "foo()");
    }

    #[test]
    fn test_cxx_call_with_args() {
        let args = vec![
            Expr::EVar { loc: loc(), name: "x".into() },
            Expr::EInt { loc: loc(), value: 1 },
        ];
        let result = cxx_call("add", &args, &[], 0);
        assert_eq!(result, "add(x, static_cast<mvp_builtin_int>(1))");
    }

    #[test]
    fn test_cxx_call_builtin_print() {
        let args = vec![
            Expr::EString { loc: loc(), value: "hello".into() },
        ];
        let result = cxx_call("print", &args, &[], 0);
        assert_eq!(result, "mvp_print(mvp_builtin_string(\"hello\"))");
    }

    // ===== cxx_if =====

    #[test]
    fn test_cxx_if_no_else() {
        let cond = Expr::EBool { loc: loc(), value: true };
        let then = Expr::EInt { loc: loc(), value: 1 };
        let e = Expr::EIf {
            loc: loc(),
            cond: Box::new(cond),
            then: Box::new(then),
            else_: None,
        };
        let result = cxx_expr(&e, 0);
        assert!(result.starts_with("([&]() { if ("));
        assert!(result.contains("true"));
        assert!(result.contains("1"));
    }

    #[test]
    fn test_cxx_if_with_else() {
        let cond = Expr::EBool { loc: loc(), value: true };
        let then = Expr::EInt { loc: loc(), value: 1 };
        let else_ = Expr::EInt { loc: loc(), value: 2 };
        let e = Expr::EIf {
            loc: loc(),
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Some(Box::new(else_)),
        };
        let result = cxx_expr(&e, 0);
        assert!(result.contains("else"));
    }

    // ===== cxx_while =====

    #[test]
    fn test_cxx_while_basic() {
        let result = cxx_while(
            &Expr::EBool { loc: loc(), value: true },
            &Expr::EVoid { loc: loc() },
            0,
        );
        assert!(result.starts_with("([&]() { while ("));
    }

    // ===== cxx_loop =====

    #[test]
    fn test_cxx_loop_basic() {
        let result = cxx_loop(&Expr::EVoid { loc: loc() }, 0);
        assert!(result.starts_with("([&]() { for (;;) {"));
    }

    // ===== cxx_for =====

    #[test]
    fn test_cxx_for_basic() {
        let result = cxx_for(
            "i",
            &Expr::EVar { loc: loc(), name: "range".into() },
            &Expr::EVoid { loc: loc() },
            0,
        );
        assert!(result.starts_with("([&]() { for (const auto& i : range) {"));
    }

    // ===== cxx_array_lit =====

    #[test]
    fn test_cxx_array_lit_empty() {
        assert_eq!(cxx_array_lit(&[], 0), "std::vector{}");
    }

    #[test]
    fn test_cxx_array_lit_values() {
        let values = vec![
            Expr::EInt { loc: loc(), value: 1 },
            Expr::EInt { loc: loc(), value: 2 },
        ];
        let result = cxx_array_lit(&values, 0);
        assert_eq!(
            result,
            "std::vector{static_cast<mvp_builtin_int>(1), static_cast<mvp_builtin_int>(2)}"
        );
    }

    // ===== cxx_stmt =====

    #[test]
    fn test_cxx_stmt_let_mutable() {
        let stmt = Stmt::SLet {
            loc: loc(),
            mutable: true,
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 5 }),
        };
        assert_eq!(
            cxx_stmt(0, &stmt),
            "auto x = static_cast<mvp_builtin_int>(5);\n"
        );
    }

    #[test]
    fn test_cxx_stmt_let_immutable() {
        let stmt = Stmt::SLet {
            loc: loc(),
            mutable: false,
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 5 }),
        };
        assert_eq!(
            cxx_stmt(0, &stmt),
            "const auto x = static_cast<mvp_builtin_int>(5);\n"
        );
    }

    #[test]
    fn test_cxx_stmt_return() {
        let stmt = Stmt::SReturn {
            loc: loc(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 0 }),
        };
        assert_eq!(
            cxx_stmt(0, &stmt),
            "return static_cast<mvp_builtin_int>(0);\n"
        );
    }

    #[test]
    fn test_cxx_stmt_expr() {
        let stmt = Stmt::SExpr {
            loc: loc(),
            expr: Box::new(Expr::ECall {
                loc: loc(),
                name: "print".to_string(),
                type_args: vec![],
                args: vec![Expr::EString {
                    loc: loc(),
                    value: "hi".into(),
                }],
            }),
        };
        assert_eq!(
            cxx_stmt(0, &stmt),
            "mvp_print(mvp_builtin_string(\"hi\"));\n"
        );
    }

    #[test]
    fn test_cxx_stmt_assign() {
        let stmt = Stmt::SAssign {
            loc: loc(),
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 10 }),
        };
        assert_eq!(
            cxx_stmt(0, &stmt),
            "x = static_cast<mvp_builtin_int>(10);\n"
        );
    }

    #[test]
    fn test_cxx_stmt_let_typed() {
        let stmt = Stmt::SLetTyped {
            loc: loc(),
            name: "x".into(),
            typ: Typ::TInt,
            expr: Box::new(Expr::EInt { loc: loc(), value: 5 }),
        };
        assert_eq!(
            cxx_stmt(0, &stmt),
            "mvp_builtin_int x = static_cast<mvp_builtin_int>(5);\n"
        );
    }
}
