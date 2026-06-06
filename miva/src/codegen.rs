use crate::ast::*;
use crate::symbol_table::SymbolTable;

fn indent_str(n: usize) -> String {
    " ".repeat(n * 2)
}

fn cxx_type(typ: &Typ) -> String {
    match typ {
        Typ::TInt => "mvp_builtin_int".into(),
        Typ::TBool => "mvp_builtin_boolean".into(),
        Typ::TFloat64 | Typ::TFloat32 => "mvp_builtin_float".into(),
        Typ::TChar => "mvp_builtin_byte".into(),
        Typ::TString => "mvp_builtin_string".into(),
        Typ::TArray { of } => format!("std::vector<{}>", cxx_type(of)),
        Typ::TStruct { name, .. } => name.clone(),
        Typ::TPtr { to } => format!("{}*", cxx_type(to)),
        Typ::TBox { of } => format!("mvp_builtin_box<{}>", cxx_type(of)),
        Typ::TNull => "void".into(),
        Typ::TPtrAny => "mvp_builtin_ptrany".into(),
        Typ::TInvalid => "invalid".into(),
    }
}

fn cxx_param(param: &Param) -> String {
    match param {
        Param::PRef { name, typ } => format!("{} const& {}", cxx_type(typ), name),
        Param::POwn { name, typ } => format!("{} {}", cxx_type(typ), name),
    }
}

fn cxx_func_decl(name: &str, params: &[Param], ret: &Option<Typ>) -> String {
    let param_list: Vec<_> = params.iter().map(cxx_param).collect();
    let ret_type = ret.as_ref().map_or("mvp_builtin_unit".into(), cxx_type);
    format!("{} {}({});\n", ret_type, name, param_list.join(", "))
}

fn map_builtin(name: &str) -> String {
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
        _ => {
            let parts: Vec<&str> = name.split('.').collect();
            if parts.first() == Some(&"ffi") {
                parts[1..].join("::")
            } else {
                parts.join("::")
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
        Expr::EChar { value, .. } => format!("'{}'", value.escape_default()),
        Expr::EString { value, .. } => format!("mvp_builtin_string(\"{}\")", value),
        Expr::EVar { name, .. } => name.clone(),
        Expr::EMove { name, .. } => format!("std::move({})", name),
        Expr::EClone { name, .. } => format!("decltype({})({})", name, name),

        Expr::EStructLit { name, fields, .. } => cxx_struct_lit(name, fields, depth),
        Expr::EFieldAccess { expr, field, .. } => {
            format!("{}.{}", cxx_expr(expr, depth), field)
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
        Expr::ECall { name, args, .. } => cxx_call(name, args, depth),
        Expr::ECast { expr, to, .. } => {
            format!("static_cast<{}>({})", cxx_type(to), cxx_expr(expr, depth))
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
    }
}

fn cxx_binop(op: &BinOp, left: &Expr, right: &Expr, depth: usize) -> String {
    let op_str = match op {
        BinOp::Add => " + ",
        BinOp::Sub => " - ",
        BinOp::Mul => " * ",
        BinOp::Eq => " == ",
        BinOp::Neq => " != ",
    };
    format!(
        "({}{}{})",
        cxx_expr(left, depth),
        op_str,
        cxx_expr(right, depth)
    )
}

fn cxx_call(name: &str, args: &[Expr], depth: usize) -> String {
    let args_strs: Vec<_> = args.iter().map(|a| cxx_expr(a, depth)).collect();
    format!("{}({})", map_builtin(name), args_strs.join(", "))
}

fn cxx_if(cond: &Expr, then: &Expr, else_: &Option<Box<Expr>>, depth: usize) -> String {
    let cond_str = cxx_expr(cond, depth);
    let then_str = cxx_expr(then, depth + 1);
    let else_str = match else_ {
        Some(e) => format!(" else {{ return {}; }}", cxx_expr(e, depth + 1)),
        None => String::new(),
    };
    format!(
        "([&]() {{ if ({}) {{ return {}; }}{} }})()",
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

fn cxx_struct_lit(name: &str, fields: &[ValueField], depth: usize) -> String {
    if fields.is_empty() {
        format!("{}{{}}", name)
    } else {
        let temp = "__temp";
        let inits: Vec<_> = fields
            .iter()
            .map(|f| format!("{}.{} = {}", temp, f.name, cxx_expr(&f.value, depth + 1)))
            .collect();
        format!(
            "([&]() {{ {} {}={{}}; {}; return {}; }}())",
            name,
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
    let cases_str: String = cases.iter().fold(String::new(), |acc, c| {
        let value_str = cxx_expr(&c.when, depth);
        let body_str = cxx_expr(&c.then, depth + 1);
        format!(
            "{}{}if ({} == {}) {{ {} }}\n",
            acc, ind, var_str, value_str, body_str
        )
    });
    let otherwise_str = match otherwise {
        Some(e) => format!("{}else {{ {} }}", ind, cxx_expr(e, depth + 1)),
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
        Stmt::SReturn { expr, .. } => {
            format!("{}return {};\n", ind, cxx_expr(expr, depth))
        }
        Stmt::SExpr { expr, .. } => {
            format!("{}{};\n", ind, cxx_expr(expr, depth))
        }
        Stmt::SAssign { name, expr, .. } => {
            format!("{}{} = {};\n", ind, name, cxx_expr(expr, depth))
        }
        Stmt::SCIntro { .. } | Stmt::SEmpty { .. } => String::new(),
    }
}

fn cxx_def(def: &Def, depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    match def {
        Def::DStruct { name, fields, .. } => cxx_struct_def(name, fields, ind, inner),
        Def::DFunc {
            name,
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
            params,
            returns,
            body,
            ..
        } => cxx_normal_func(name, params, returns, body, ind, inner),
        Def::DImpl {
            struct_name, impls, ..
        } => cxx_impl(struct_name, impls, ind),
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

fn cxx_struct_def(name: &str, fields: &[FieldDef], ind: String, inner: usize) -> String {
    let field_strs: String = fields
        .iter()
        .map(|f| format!("{}{} {};\n", indent_str(inner), cxx_type(&f.typ), f.name))
        .collect();
    format!("struct {} {{\n{}{}}};\n\n", name, field_strs, ind)
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
    let signature = format!("{} {}({})", ret_type, name, param_strs.join(", "));
    format!("{} {} {{\n{}{}}}\n\n", ind, signature, code, ind)
}

fn cxx_normal_func(
    name: &str,
    params: &[Param],
    returns: &Option<Typ>,
    body: &Expr,
    ind: String,
    _inner: usize,
) -> String {
    let param_strs: Vec<_> = params.iter().map(cxx_param).collect();
    let ret_type = returns.as_ref().map_or("mvp_builtin_unit".into(), cxx_type);
    let signature = format!("{} {}({})", ret_type, name, param_strs.join(", "));

    match body {
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
    }
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
                    format!("#include <src/{}.h>\n", parts[1..].join("/"))
                } else {
                    String::new()
                }
            } else if let Some(head) = parts.first() {
                if parts.len() > 1 {
                    format!("#include <{}/src/{}.h>\n", head, parts[1..].join("/"))
                } else {
                    format!("#include <{}.h>\n", head)
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
        let parts: Vec<&str> = path.split('/').collect();
        if let Some(head) = parts.first() {
            format!("{}using namespace {};\n", include, head.replace('.', "::"))
        } else {
            include
        }
    }
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
    format!("#pragma once\n\n#include <mvp_builtin.h>\n\n{}\n", exported)
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
                    let field_strs: String = s
                        .fields
                        .iter()
                        .map(|f| format!("  {} {};\n", cxx_type(&f.typ), f.name))
                        .collect();
                    format!("struct {} {{\n{}}};\n\n", s.name, field_strs)
                } else if let Some(f) = sym.lookup_function(symbol) {
                    cxx_func_decl(&f.name, &f.params, &f.return_typ)
                } else {
                    String::new()
                };
                result.push_str(&decl);
            }
            _ => {}
        }
    }
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
            Def::SImport { path, .. } => includes.push_str(&cxx_include_path(path)),
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
        let typ = Typ::TArray { of: Box::new(Typ::TInt) };
        assert_eq!(cxx_type(&typ), "std::vector<mvp_builtin_int>");
    }

    #[test]
    fn test_cxx_type_nested_array() {
        let inner = Typ::TArray { of: Box::new(Typ::TInt) };
        let outer = Typ::TArray { of: Box::new(inner) };
        assert_eq!(cxx_type(&outer), "std::vector<std::vector<mvp_builtin_int>>");
    }

    #[test]
    fn test_cxx_type_struct() {
        let typ = Typ::TStruct { name: "Point".into(), fields: vec![] };
        assert_eq!(cxx_type(&typ), "Point");
    }

    #[test]
    fn test_cxx_type_ptr() {
        let typ = Typ::TPtr { to: Box::new(Typ::TInt) };
        assert_eq!(cxx_type(&typ), "mvp_builtin_int*");
    }

    #[test]
    fn test_cxx_type_box() {
        let typ = Typ::TBox { of: Box::new(Typ::TInt) };
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
        let p = Param::PRef { name: "x".into(), typ: Typ::TInt };
        assert_eq!(cxx_param(&p), "mvp_builtin_int const& x");
    }

    #[test]
    fn test_cxx_param_own() {
        let p = Param::POwn { name: "flag".into(), typ: Typ::TBool };
        assert_eq!(cxx_param(&p), "mvp_builtin_boolean flag");
    }

    #[test]
    fn test_cxx_param_ref_string() {
        let p = Param::PRef { name: "s".into(), typ: Typ::TString };
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
            Param::POwn { name: "a".into(), typ: Typ::TInt },
            Param::POwn { name: "b".into(), typ: Typ::TInt },
        ];
        let result = cxx_func_decl("add", &params, &Some(Typ::TInt));
        assert_eq!(result, "mvp_builtin_int add(mvp_builtin_int a, mvp_builtin_int b);\n");
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
        let e = Expr::EInt { loc: loc(), value: 42 };
        assert_eq!(cxx_expr(&e, 0), "static_cast<mvp_builtin_int>(42)");
    }

    #[test]
    fn test_cxx_expr_neg_int() {
        let e = Expr::EInt { loc: loc(), value: -5 };
        assert_eq!(cxx_expr(&e, 0), "static_cast<mvp_builtin_int>(-5)");
    }

    #[test]
    fn test_cxx_expr_bool_true() {
        let e = Expr::EBool { loc: loc(), value: true };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_boolean(true)");
    }

    #[test]
    fn test_cxx_expr_bool_false() {
        let e = Expr::EBool { loc: loc(), value: false };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_boolean(false)");
    }

    #[test]
    fn test_cxx_expr_float() {
        let e = Expr::EFloat { loc: loc(), value: 3.14 };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_float(3.14)");
    }

    #[test]
    fn test_cxx_expr_float_zero() {
        let e = Expr::EFloat { loc: loc(), value: 0.0 };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_float(0)");
    }

    #[test]
    fn test_cxx_expr_char() {
        let e = Expr::EChar { loc: loc(), value: "a".into() };
        assert_eq!(cxx_expr(&e, 0), "'a'");
    }

    #[test]
    fn test_cxx_expr_string() {
        let e = Expr::EString { loc: loc(), value: "hello".into() };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_string(\"hello\")");
    }

    #[test]
    fn test_cxx_expr_var() {
        let e = Expr::EVar { loc: loc(), name: "x".into() };
        assert_eq!(cxx_expr(&e, 0), "x");
    }

    #[test]
    fn test_cxx_expr_move() {
        let e = Expr::EMove { loc: loc(), name: "x".into() };
        assert_eq!(cxx_expr(&e, 0), "std::move(x)");
    }

    #[test]
    fn test_cxx_expr_clone() {
        let e = Expr::EClone { loc: loc(), name: "x".into() };
        assert_eq!(cxx_expr(&e, 0), "decltype(x)(x)");
    }

    #[test]
    fn test_cxx_expr_void() {
        let e = Expr::EVoid { loc: loc() };
        assert_eq!(cxx_expr(&e, 0), "mvp_builtin_void");
    }

    #[test]
    fn test_cxx_expr_addr() {
        let e = Expr::EAddr { loc: loc(), expr: Box::new(Expr::EVar { loc: loc(), name: "x".into() }) };
        assert_eq!(cxx_expr(&e, 0), "&(x)");
    }

    #[test]
    fn test_cxx_expr_deref() {
        let e = Expr::EDeref { loc: loc(), expr: Box::new(Expr::EVar { loc: loc(), name: "p".into() }) };
        assert_eq!(cxx_expr(&e, 0), "*(p)");
    }

    #[test]
    fn test_cxx_expr_macro_empty() {
        let e = Expr::EMacro { loc: loc(), name: "something".into(), args: vec![] };
        assert_eq!(cxx_expr(&e, 0), "");
    }

    #[test]
    fn test_cxx_expr_field_access() {
        let e = Expr::EFieldAccess {
            loc: loc(),
            expr: Box::new(Expr::EVar { loc: loc(), name: "p".into() }),
            field: "x".into(),
        };
        assert_eq!(cxx_expr(&e, 0), "p.x");
    }

    #[test]
    fn test_cxx_expr_cast() {
        let e = Expr::ECast {
            loc: loc(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 65 }),
            to: Typ::TChar,
        };
        assert_eq!(cxx_expr(&e, 0), "static_cast<mvp_builtin_byte>(static_cast<mvp_builtin_int>(65))");
    }

    // ===== cxx_binop =====

    #[test]
    fn test_cxx_binop_add() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp { loc: loc(), op: BinOp::Add, left: Box::new(left), right: Box::new(right) };
        assert_eq!(cxx_expr(&e, 0), "(a + b)");
    }

    #[test]
    fn test_cxx_binop_sub() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp { loc: loc(), op: BinOp::Sub, left: Box::new(left), right: Box::new(right) };
        assert_eq!(cxx_expr(&e, 0), "(a - b)");
    }

    #[test]
    fn test_cxx_binop_mul() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp { loc: loc(), op: BinOp::Mul, left: Box::new(left), right: Box::new(right) };
        assert_eq!(cxx_expr(&e, 0), "(a * b)");
    }

    #[test]
    fn test_cxx_binop_eq() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp { loc: loc(), op: BinOp::Eq, left: Box::new(left), right: Box::new(right) };
        assert_eq!(cxx_expr(&e, 0), "(a == b)");
    }

    #[test]
    fn test_cxx_binop_neq() {
        let left = Expr::EVar { loc: loc(), name: "a".into() };
        let right = Expr::EVar { loc: loc(), name: "b".into() };
        let e = Expr::EBinOp { loc: loc(), op: BinOp::Neq, left: Box::new(left), right: Box::new(right) };
        assert_eq!(cxx_expr(&e, 0), "(a != b)");
    }

    // ===== cxx_call =====

    #[test]
    fn test_cxx_call_builtin_no_args() {
        let e = Expr::ECall { loc: loc(), name: "println".into(), args: vec![] };
        assert_eq!(cxx_expr(&e, 0), "mvp_println()");
    }

    #[test]
    fn test_cxx_call_builtin_one_arg() {
        let e = Expr::ECall {
            loc: loc(),
            name: "printlns".into(),
            args: vec![Expr::EString { loc: loc(), value: "hi".into() }],
        };
        assert_eq!(cxx_expr(&e, 0), "mvp_printlns(mvp_builtin_string(\"hi\"))");
    }

    #[test]
    fn test_cxx_call_user_func() {
        let e = Expr::ECall {
            loc: loc(),
            name: "myfunc".into(),
            args: vec![Expr::EInt { loc: loc(), value: 1 }, Expr::EInt { loc: loc(), value: 2 }],
        };
        assert_eq!(cxx_expr(&e, 0), "myfunc(static_cast<mvp_builtin_int>(1), static_cast<mvp_builtin_int>(2))");
    }

    #[test]
    fn test_cxx_call_namespaced() {
        let e = Expr::ECall {
            loc: loc(),
            name: "math.add".into(),
            args: vec![],
        };
        assert_eq!(cxx_expr(&e, 0), "math::add()");
    }

    // ===== cxx_if =====

    #[test]
    fn test_cxx_if_no_else() {
        let e = Expr::EIf {
            loc: loc(),
            cond: Box::new(Expr::EBool { loc: loc(), value: true }),
            then: Box::new(Expr::EInt { loc: loc(), value: 1 }),
            else_: None,
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() { if (mvp_builtin_boolean(true)) { return static_cast<mvp_builtin_int>(1); } })()"
        );
    }

    #[test]
    fn test_cxx_if_with_else() {
        let e = Expr::EIf {
            loc: loc(),
            cond: Box::new(Expr::EBool { loc: loc(), value: true }),
            then: Box::new(Expr::EInt { loc: loc(), value: 1 }),
            else_: Some(Box::new(Expr::EInt { loc: loc(), value: 2 })),
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() { if (mvp_builtin_boolean(true)) { return static_cast<mvp_builtin_int>(1); } else { return static_cast<mvp_builtin_int>(2); } })()"
        );
    }

    // ===== cxx_while =====

    #[test]
    fn test_cxx_while() {
        let e = Expr::EWhile {
            loc: loc(),
            cond: Box::new(Expr::EBool { loc: loc(), value: true }),
            body: Box::new(Expr::EVoid { loc: loc() }),
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() { while (mvp_builtin_boolean(true)) { mvp_builtin_void ;}})()"
        );
    }

    // ===== cxx_loop =====

    #[test]
    fn test_cxx_loop() {
        let e = Expr::ELoop {
            loc: loc(),
            body: Box::new(Expr::EVoid { loc: loc() }),
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() { for (;;) { mvp_builtin_void ;}})()"
        );
    }

    // ===== cxx_for =====

    #[test]
    fn test_cxx_for() {
        let e = Expr::EFor {
            loc: loc(),
            var: "i".into(),
            range: Box::new(Expr::EVar { loc: loc(), name: "items".into() }),
            body: Box::new(Expr::EVoid { loc: loc() }),
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() { for (const auto& i : items) { mvp_builtin_void ;}})()"
        );
    }

    // ===== cxx_struct_lit =====

    #[test]
    fn test_cxx_struct_lit_empty() {
        let e = Expr::EStructLit {
            loc: loc(),
            name: "Empty".into(),
            fields: vec![],
        };
        assert_eq!(cxx_expr(&e, 0), "Empty{}");
    }

    #[test]
    fn test_cxx_struct_lit_with_fields() {
        let e = Expr::EStructLit {
            loc: loc(),
            name: "Point".into(),
            fields: vec![
                ValueField { name: "x".into(), value: Expr::EInt { loc: loc(), value: 1 } },
                ValueField { name: "y".into(), value: Expr::EInt { loc: loc(), value: 2 } },
            ],
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() { Point __temp={}; __temp.x = static_cast<mvp_builtin_int>(1); __temp.y = static_cast<mvp_builtin_int>(2); return __temp; }())"
        );
    }

    // ===== cxx_choose =====

    #[test]
    fn test_cxx_choose_single_case() {
        let e = Expr::EChoose {
            loc: loc(),
            var: Box::new(Expr::EVar { loc: loc(), name: "x".into() }),
            cases: vec![WhenCase {
                when: Box::new(Expr::EInt { loc: loc(), value: 1 }),
                then: Box::new(Expr::EInt { loc: loc(), value: 10 }),
            }],
            otherwise: None,
        };
        let result = cxx_expr(&e, 0);
        assert!(result.starts_with("([&]() {"));
        assert!(result.contains("if (x == static_cast<mvp_builtin_int>(1))"));
        assert!(result.contains("static_cast<mvp_builtin_int>(10)"));
    }

    #[test]
    fn test_cxx_choose_with_otherwise() {
        let e = Expr::EChoose {
            loc: loc(),
            var: Box::new(Expr::EVar { loc: loc(), name: "x".into() }),
            cases: vec![WhenCase {
                when: Box::new(Expr::EInt { loc: loc(), value: 1 }),
                then: Box::new(Expr::EInt { loc: loc(), value: 10 }),
            }],
            otherwise: Some(Box::new(Expr::EInt { loc: loc(), value: 0 })),
        };
        let result = cxx_expr(&e, 0);
        assert!(result.contains("else"));
        assert!(result.contains("static_cast<mvp_builtin_int>(0)"));
    }

    // ===== cxx_array_lit =====

    #[test]
    fn test_cxx_array_lit_empty() {
        let e = Expr::EArrayLit { loc: loc(), values: vec![] };
        assert_eq!(cxx_expr(&e, 0), "std::vector{}");
    }

    #[test]
    fn test_cxx_array_lit_with_values() {
        let e = Expr::EArrayLit {
            loc: loc(),
            values: vec![
                Expr::EInt { loc: loc(), value: 1 },
                Expr::EInt { loc: loc(), value: 2 },
            ],
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "std::vector{static_cast<mvp_builtin_int>(1), static_cast<mvp_builtin_int>(2)}"
        );
    }

    // ===== cxx_block =====

    #[test]
    fn test_cxx_block_empty() {
        let e = Expr::EBlock { loc: loc(), stmts: vec![], result: None };
        assert_eq!(cxx_expr(&e, 0), "([&]() {\n})()");
    }

    #[test]
    fn test_cxx_block_with_let() {
        let e = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SLet {
                loc: loc(),
                mutable: false,
                name: "x".into(),
                expr: Box::new(Expr::EInt { loc: loc(), value: 42 }),
            }],
            result: Some(Box::new(Expr::EVar { loc: loc(), name: "x".into() })),
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() {\n  const auto x = static_cast<mvp_builtin_int>(42);\n  return x;\n})()"
        );
    }

    #[test]
    fn test_cxx_block_mutable_let() {
        let e = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SLet {
                loc: loc(),
                mutable: true,
                name: "x".into(),
                expr: Box::new(Expr::EInt { loc: loc(), value: 0 }),
            }],
            result: None,
        };
        assert_eq!(
            cxx_expr(&e, 0),
            "([&]() {\n  auto x = static_cast<mvp_builtin_int>(0);\n})()"
        );
    }

    #[test]
    fn test_cxx_block_with_expr_stmt() {
        let e = Expr::EBlock {
            loc: loc(),
            stmts: vec![
                Stmt::SLet {
                    loc: loc(),
                    mutable: false,
                    name: "x".into(),
                    expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
                },
                Stmt::SExpr {
                    loc: loc(),
                    expr: Box::new(Expr::ECall {
                        loc: loc(),
                        name: "println".into(),
                        args: vec![Expr::EVar { loc: loc(), name: "x".into() }],
                    }),
                },
            ],
            result: Some(Box::new(Expr::EVoid { loc: loc() })),
        };
        let result = cxx_expr(&e, 0);
        assert!(result.contains("const auto x = static_cast<mvp_builtin_int>(1)"));
        assert!(result.contains("mvp_println(x)"));
        assert!(result.contains("return mvp_builtin_void"));
    }

    // ===== cxx_stmt =====

    #[test]
    fn test_cxx_stmt_let_immutable() {
        let s = Stmt::SLet {
            loc: loc(),
            mutable: false,
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 5 }),
        };
        assert_eq!(cxx_stmt(1, &s), "  const auto x = static_cast<mvp_builtin_int>(5);\n");
    }

    #[test]
    fn test_cxx_stmt_let_mutable() {
        let s = Stmt::SLet {
            loc: loc(),
            mutable: true,
            name: "counter".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 0 }),
        };
        assert_eq!(cxx_stmt(1, &s), "  auto counter = static_cast<mvp_builtin_int>(0);\n");
    }

    #[test]
    fn test_cxx_stmt_return() {
        let s = Stmt::SReturn {
            loc: loc(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 42 }),
        };
        assert_eq!(cxx_stmt(0, &s), "return static_cast<mvp_builtin_int>(42);\n");
    }

    #[test]
    fn test_cxx_stmt_expr() {
        let s = Stmt::SExpr {
            loc: loc(),
            expr: Box::new(Expr::ECall {
                loc: loc(),
                name: "println".into(),
                args: vec![],
            }),
        };
        assert_eq!(cxx_stmt(0, &s), "mvp_println();\n");
    }

    #[test]
    fn test_cxx_stmt_assign() {
        let s = Stmt::SAssign {
            loc: loc(),
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 10 }),
        };
        assert_eq!(cxx_stmt(0, &s), "x = static_cast<mvp_builtin_int>(10);\n");
    }

    #[test]
    fn test_cxx_stmt_cintro_empty() {
        let s = Stmt::SCIntro { loc: loc(), content: "some c code".into() };
        assert_eq!(cxx_stmt(0, &s), "");
    }

    #[test]
    fn test_cxx_stmt_empty() {
        let s = Stmt::SEmpty { loc: loc() };
        assert_eq!(cxx_stmt(0, &s), "");
    }

    #[test]
    fn test_cxx_stmt_depth_handling() {
        let s = Stmt::SLet {
            loc: loc(),
            mutable: false,
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
        };
        assert_eq!(cxx_stmt(2, &s), "    const auto x = static_cast<mvp_builtin_int>(1);\n");
    }

    // ===== analyze_block =====

    #[test]
    fn test_analyze_block_non_block_returns_none() {
        let e = Expr::EInt { loc: loc(), value: 1 };
        assert!(analyze_block(&e, 0, "mvp_builtin_int").is_none());
    }

    #[test]
    fn test_analyze_block_unit_return() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SLet {
                loc: loc(),
                mutable: false,
                name: "x".into(),
                expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
            }],
            result: None,
        };
        let flow = analyze_block(&body, 0, "mvp_builtin_unit").unwrap();
        assert_eq!(flow.stmts.len(), 1);
        assert_eq!(flow.ret_line, "  return mvp_builtin_void;\n");
    }

    #[test]
    fn test_analyze_block_typed_return_with_result() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![],
            result: Some(Box::new(Expr::EInt { loc: loc(), value: 42 })),
        };
        let flow = analyze_block(&body, 0, "mvp_builtin_int").unwrap();
        assert!(flow.ret_line.contains("return static_cast<mvp_builtin_int>(42)"));
    }

    #[test]
    fn test_analyze_block_typed_return_last_expr() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![
                Stmt::SLet {
                    loc: loc(),
                    mutable: false,
                    name: "x".into(),
                    expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
                },
                Stmt::SExpr {
                    loc: loc(),
                    expr: Box::new(Expr::EVar { loc: loc(), name: "x".into() }),
                },
            ],
            result: None,
        };
        let flow = analyze_block(&body, 0, "mvp_builtin_int").unwrap();
        assert_eq!(flow.stmts.len(), 1);
        assert!(flow.ret_line.contains("return x"));
    }

    // ===== take_last_expr =====

    #[test]
    fn test_take_last_expr_last_is_expr() {
        let stmts = vec![
            Stmt::SLet {
                loc: loc(),
                mutable: false,
                name: "x".into(),
                expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
            },
            Stmt::SExpr {
                loc: loc(),
                expr: Box::new(Expr::EVar { loc: loc(), name: "x".into() }),
            },
        ];
        let (result_stmts, last_expr) = take_last_expr(&stmts, 0);
        assert_eq!(result_stmts.len(), 1);
        assert!(last_expr.is_some());
        if let Some(expr) = last_expr {
            assert_eq!(cxx_expr(&expr, 0), "x");
        }
    }

    #[test]
    fn test_take_last_expr_last_is_not_expr() {
        let stmts = vec![Stmt::SLet {
            loc: loc(),
            mutable: false,
            name: "x".into(),
            expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
        }];
        let (result_stmts, last_expr) = take_last_expr(&stmts, 0);
        assert_eq!(result_stmts.len(), 1);
        assert!(last_expr.is_none());
    }

    #[test]
    fn test_take_last_expr_empty() {
        let stmts = vec![];
        let (result_stmts, last_expr) = take_last_expr(&stmts, 0);
        assert!(result_stmts.is_empty());
        assert!(last_expr.is_none());
    }

    // ===== is_main_func =====

    #[test]
    fn test_is_main_func_matches() {
        let def = Def::DFunc {
            loc: loc(),
            name: "main".into(),
            params: vec![],
            returns: None,
            body: Box::new(Expr::EVoid { loc: loc() }),
            safety: Safety::Safe,
        };
        assert!(is_main_func(&def));
    }

    #[test]
    fn test_is_main_func_not_main() {
        let def = Def::DFunc {
            loc: loc(),
            name: "foo".into(),
            params: vec![],
            returns: None,
            body: Box::new(Expr::EVoid { loc: loc() }),
            safety: Safety::Safe,
        };
        assert!(!is_main_func(&def));
    }

    #[test]
    fn test_is_main_func_non_func() {
        let def = Def::DStruct { loc: loc(), name: "Foo".into(), fields: vec![] };
        assert!(!is_main_func(&def));
    }

    // ===== cxx_struct_def =====

    #[test]
    fn test_cxx_struct_def_empty() {
        let result = cxx_struct_def("Empty", &[], "".into(), 0);
        assert_eq!(result, "struct Empty {\n};\n\n");
    }

    #[test]
    fn test_cxx_struct_def_with_fields() {
        let fields = vec![
            FieldDef { name: "x".into(), typ: Typ::TInt },
            FieldDef { name: "y".into(), typ: Typ::TBool },
        ];
        let result = cxx_struct_def("Point", &fields, "".into(), 1);
        assert_eq!(result, "struct Point {\n  mvp_builtin_int x;\n  mvp_builtin_boolean y;\n};\n\n");
    }

    // ===== cxx_func helpers =====

    #[test]
    fn test_cxx_main_func_block_body() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SLet {
                loc: loc(),
                mutable: false,
                name: "x".into(),
                expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
            }],
            result: None,
        };
        let result = cxx_main_func("".into(), 0, &body);
        assert!(result.contains("mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)"));
        assert!(result.contains("const auto x = static_cast<mvp_builtin_int>(1)"));
        assert!(result.contains("return mvp_builtin_void"));
    }

    #[test]
    fn test_cxx_main_func_expr_body() {
        let body = Expr::ECall {
            loc: loc(),
            name: "println".into(),
            args: vec![],
        };
        let result = cxx_main_func("".into(), 0, &body);
        assert!(result.contains("mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)"));
        assert!(result.contains("mvp_println()"));
        assert!(result.contains("return mvp_builtin_void"));
    }

    // ===== cxx_cfunc =====

    #[test]
    fn test_cxx_cfunc_no_params() {
        let result = cxx_cfunc("my_c_func", &[], &None, "  // custom code\n", "".into());
        assert_eq!(result, " mvp_builtin_unit my_c_func() {\n  // custom code\n}\n\n");
    }

    #[test]
    fn test_cxx_cfunc_with_params_and_return() {
        let params = vec![Param::POwn { name: "x".into(), typ: Typ::TInt }];
        let result = cxx_cfunc("add_one", &params, &Some(Typ::TInt), "  return x + 1;\n", "".into());
        assert_eq!(result, " mvp_builtin_int add_one(mvp_builtin_int x) {\n  return x + 1;\n}\n\n");
    }

    // ===== cxx_normal_func =====

    #[test]
    fn test_cxx_normal_func_unit_return_block() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SExpr {
                loc: loc(),
                expr: Box::new(Expr::ECall {
                    loc: loc(),
                    name: "println".into(),
                    args: vec![Expr::EString { loc: loc(), value: "hi".into() }],
                }),
            }],
            result: None,
        };
        let result = cxx_normal_func("greet", &[], &None, &body, "".into(), 0);
        assert!(result.contains("mvp_builtin_unit greet()"));
        assert!(result.contains("mvp_println(mvp_builtin_string(\"hi\"))"));
        assert!(result.contains("return mvp_builtin_void"));
    }

    #[test]
    fn test_cxx_normal_func_typed_return_block() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![],
            result: Some(Box::new(Expr::EInt { loc: loc(), value: 42 })),
        };
        let result = cxx_normal_func("answer", &[], &Some(Typ::TInt), &body, "".into(), 0);
        assert!(result.contains("mvp_builtin_int answer()"));
        assert!(result.contains("return static_cast<mvp_builtin_int>(42)"));
    }

    #[test]
    fn test_cxx_normal_func_unit_expr_body() {
        let body = Expr::ECall {
            loc: loc(),
            name: "println".into(),
            args: vec![],
        };
        let result = cxx_normal_func("do_it", &[], &None, &body, "".into(), 0);
        assert_eq!(result, " mvp_builtin_unit do_it() { mvp_println(); return mvp_builtin_void; }\n\n");
    }

    #[test]
    fn test_cxx_normal_func_typed_expr_body() {
        let body = Expr::EInt { loc: loc(), value: 99 };
        let result = cxx_normal_func("get_val", &[], &Some(Typ::TInt), &body, "".into(), 0);
        assert_eq!(result, " mvp_builtin_int get_val() { return static_cast<mvp_builtin_int>(99); }\n\n");
    }

    // ===== cxx_impl =====

    #[test]
    fn test_cxx_impl_add() {
        let impl_expr = ImplExpr { op: ImplOp::ImAdd, func: "vec_add".into(), loc: loc() };
        let result = cxx_impl("Vec2", &[impl_expr], "".into());
        assert!(result.contains("Vec2 operator+(const Vec2& ____a, const Vec2& ____b) { return vec_add(____a, ____b); }"));
    }

    #[test]
    fn test_cxx_impl_sub() {
        let impl_expr = ImplExpr { op: ImplOp::ImSub, func: "vec_sub".into(), loc: loc() };
        let result = cxx_impl("Vec2", &[impl_expr], "".into());
        assert!(result.contains("operator-"));
    }

    #[test]
    fn test_cxx_impl_mul() {
        let impl_expr = ImplExpr { op: ImplOp::ImMul, func: "vec_mul".into(), loc: loc() };
        let result = cxx_impl("Vec2", &[impl_expr], "".into());
        assert!(result.contains("operator*"));
    }

    #[test]
    fn test_cxx_impl_eq() {
        let impl_expr = ImplExpr { op: ImplOp::ImEq, func: "vec_eq".into(), loc: loc() };
        let result = cxx_impl("Vec2", &[impl_expr], "".into());
        assert!(result.contains("mvp_builtin_boolean operator=="));
    }

    #[test]
    fn test_cxx_impl_neq() {
        let impl_expr = ImplExpr { op: ImplOp::ImNeq, func: "vec_neq".into(), loc: loc() };
        let result = cxx_impl("Vec2", &[impl_expr], "".into());
        assert!(result.contains("mvp_builtin_boolean operator!="));
    }

    #[test]
    fn test_cxx_impl_multi() {
        let impls = vec![
            ImplExpr { op: ImplOp::ImAdd, func: "add".into(), loc: loc() },
            ImplExpr { op: ImplOp::ImEq, func: "eq".into(), loc: loc() },
        ];
        let result = cxx_impl("Foo", &impls, "".into());
        assert!(result.contains("operator+"));
        assert!(result.contains("operator=="));
    }

    // ===== cxx_include_path =====

    #[test]
    fn test_cxx_include_path_c_header() {
        let result = cxx_include_path("c:stdio");
        assert_eq!(result, "#include <stdio>\n");
    }

    #[test]
    fn test_cxx_include_path_no_project_name() {
        // Without miva.toml, project_name() returns None -> empty string
        let result = cxx_include_path("some/path");
        assert_eq!(result, "");
    }

    // ===== cxx_include_here =====

    #[test]
    fn test_cxx_include_here_c_header() {
        let result = cxx_include_here("c:stdio");
        assert_eq!(result, "#include <stdio>\n");
    }

    // ===== generate_test =====

    #[test]
    fn test_generate_test_no_tests() {
        let defs = vec![];
        assert_eq!(generate_test(&defs), "");
    }

    #[test]
    fn test_generate_test_simple() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "test_mod".into() },
            Def::DTest {
                loc: loc(),
                name: "test_one".into(),
                body: Box::new(Expr::EInt { loc: loc(), value: 0 }),
            },
        ];
        let result = generate_test(&defs);
        assert!(result.contains("#include <mvp_test.h>"));
        assert!(result.contains("mvp_builtin_int test_one()"));
        assert!(result.contains("return static_cast<mvp_builtin_int>(0)"));
        assert!(result.contains("\"test_one\""));
        assert!(result.contains("mvp_run_tests"));
    }

    #[test]
    fn test_generate_test_block_body() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "test_mod".into() },
            Def::DTest {
                loc: loc(),
                name: "test_block".into(),
                body: Box::new(Expr::EBlock {
                    loc: loc(),
                    stmts: vec![],
                    result: Some(Box::new(Expr::EInt { loc: loc(), value: 1 })),
                }),
            },
        ];
        let result = generate_test(&defs);
        assert!(result.contains("test_block()"));
        assert!(result.contains("return static_cast<mvp_builtin_int>(1)"));
    }

    #[test]
    fn test_generate_test_multiple() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "m".into() },
            Def::DTest {
                loc: loc(),
                name: "t1".into(),
                body: Box::new(Expr::EInt { loc: loc(), value: 0 }),
            },
            Def::DTest {
                loc: loc(),
                name: "t2".into(),
                body: Box::new(Expr::EInt { loc: loc(), value: 0 }),
            },
        ];
        let result = generate_test(&defs);
        assert!(result.contains("\"t1\", t1}"));
        assert!(result.contains("\"t2\", t2}"));
        assert!(result.contains("sizeof(tests) / sizeof(tests[0])"));
    }

    // ===== generate_header =====

    #[test]
    fn test_generate_header_no_exports() {
        let defs = vec![Def::DModule { loc: loc(), name: "empty".into() }];
        let hdr = generate_header(&defs);
        assert!(hdr.contains("#pragma once"));
        assert!(hdr.contains("namespace empty {"));
        assert!(!hdr.contains("mvp_builtin_int"));
    }

    // ===== build_ir integration =====

    #[test]
    fn test_build_ir_empty() {
        let [program, header, test] = build_ir(&[]);
        assert!(program.starts_with("#include <iostream>"));
        assert!(header.is_empty());
        assert!(test.is_empty());
    }

    #[test]
    fn test_build_ir_simple_func() {
        let defs = vec![
            Def::DFunc {
                loc: loc(),
                name: "foo".into(),
                params: vec![],
                returns: None,
                body: Box::new(Expr::EVoid { loc: loc() }),
                safety: Safety::Safe,
            },
        ];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("mvp_builtin_unit foo()"));
        assert!(program.contains("return mvp_builtin_void"));
    }

    #[test]
    fn test_build_ir_main_func() {
        let defs = vec![Def::DFunc {
            loc: loc(),
            name: "main".into(),
            params: vec![],
            returns: None,
            body: Box::new(Expr::EBlock {
                loc: loc(),
                stmts: vec![],
                result: None,
            }),
            safety: Safety::Safe,
        }];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("mvp_own_main"));
        assert!(program.contains("int main(int argc, char** argv)"));
    }

    #[test]
    fn test_build_ir_with_module_and_main() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "myapp".into() },
            Def::DFunc {
                loc: loc(),
                name: "main".into(),
                params: vec![],
                returns: None,
                body: Box::new(Expr::EBlock {
                    loc: loc(),
                    stmts: vec![],
                    result: None,
                }),
                safety: Safety::Safe,
            },
        ];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("namespace myapp {"));
        assert!(program.contains("myapp::mvp_own_main"));
    }

    #[test]
    fn test_build_ir_with_import() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "app".into() },
            Def::SImport { loc: loc(), path: "c:iostream".into() },
        ];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("#include <iostream>"));
    }

    #[test]
    fn test_build_ir_with_export() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "lib".into() },
            Def::DFunc {
                loc: loc(),
                name: "add".into(),
                params: vec![
                    Param::POwn { name: "a".into(), typ: Typ::TInt },
                    Param::POwn { name: "b".into(), typ: Typ::TInt },
                ],
                returns: Some(Typ::TInt),
                body: Box::new(Expr::EBinOp {
                    loc: loc(),
                    op: BinOp::Add,
                    left: Box::new(Expr::EVar { loc: loc(), name: "a".into() }),
                    right: Box::new(Expr::EVar { loc: loc(), name: "b".into() }),
                }),
                safety: Safety::Safe,
            },
            Def::SExport { loc: loc(), symbol: "add".into() },
        ];
        let [_program, header, _test] = build_ir(&defs);
        assert!(header.contains("#pragma once"));
        assert!(header.contains("mvp_builtin_int add("));
    }

    #[test]
    fn test_build_ir_with_struct() {
        let defs = vec![
            Def::DStruct {
                loc: loc(),
                name: "Point".into(),
                fields: vec![
                    FieldDef { name: "x".into(), typ: Typ::TInt },
                    FieldDef { name: "y".into(), typ: Typ::TInt },
                ],
            },
        ];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("struct Point {"));
        assert!(program.contains("mvp_builtin_int x;"));
        assert!(program.contains("mvp_builtin_int y;"));
    }

    #[test]
    fn test_build_ir_with_cfunc() {
        let defs = vec![Def::DCFuncUnsafe {
            loc: loc(),
            name: "c_func".into(),
            params: vec![],
            returns: None,
            code: "  return mvp_builtin_void;\n".into(),
            safety: Safety::Unsafe,
        }];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("c_func()"));
        assert!(program.contains("return mvp_builtin_void"));
    }

    #[test]
    fn test_build_ir_with_impl() {
        let defs = vec![Def::DImpl {
            loc: loc(),
            struct_name: "Vec2".into(),
            impls: vec![ImplExpr {
                op: ImplOp::ImAdd,
                func: "vec_add".into(),
                loc: loc(),
            }],
        }];
        let [program, _header, _test] = build_ir(&defs);
        assert!(program.contains("operator+"));
    }

    #[test]
    fn test_build_ir_with_test() {
        let defs = vec![
            Def::DModule { loc: loc(), name: "app".into() },
            Def::DTest {
                loc: loc(),
                name: "my_test".into(),
                body: Box::new(Expr::EInt { loc: loc(), value: 0 }),
            },
        ];
        let [_program, _header, test] = build_ir(&defs);
        assert!(test.contains("my_test()"));
        assert!(test.contains("return static_cast<mvp_builtin_int>(0)"));
    }

    // ===== cxx_def edge cases =====

    #[test]
    fn test_cxx_def_module_returns_empty() {
        let def = Def::DModule { loc: loc(), name: "m".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_export_returns_empty() {
        let def = Def::SExport { loc: loc(), symbol: "foo".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_import_returns_empty() {
        let def = Def::SImport { loc: loc(), path: "std/io".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_import_as_returns_empty() {
        let def = Def::SImportAs { loc: loc(), path: "std/io".into(), alias: "io".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_import_here_returns_empty() {
        let def = Def::SImportHere { loc: loc(), path: "std/io".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_test_returns_empty() {
        let def = Def::DTest {
            loc: loc(),
            name: "t".into(),
            body: Box::new(Expr::EVoid { loc: loc() }),
        };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_cmagical_returns_empty() {
        let def = Def::DCMagical { loc: loc(), content: "something".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    #[test]
    fn test_cxx_def_cintro_returns_empty() {
        let def = Def::DCIntro { loc: loc(), content: "something".into() };
        assert_eq!(cxx_def(&def, 0), "");
    }

    // ===== cxx_normal_func with params =====

    #[test]
    fn test_cxx_normal_func_with_params() {
        let params = vec![
            Param::POwn { name: "a".into(), typ: Typ::TInt },
            Param::POwn { name: "b".into(), typ: Typ::TInt },
        ];
        let body = Expr::EInt { loc: loc(), value: 0 };
        let result = cxx_normal_func("noop", &params, &None, &body, "".into(), 0);
        assert!(result.contains("mvp_builtin_unit noop(mvp_builtin_int a, mvp_builtin_int b)"));
    }

    // ===== cxx_struct_def with non-int fields =====

    #[test]
    fn test_cxx_struct_def_varied_types() {
        let fields = vec![
            FieldDef { name: "name".into(), typ: Typ::TString },
            FieldDef { name: "age".into(), typ: Typ::TInt },
            FieldDef { name: "active".into(), typ: Typ::TBool },
        ];
        let result = cxx_struct_def("Person", &fields, "".into(), 0);
        assert!(result.contains("mvp_builtin_string name;"));
        assert!(result.contains("mvp_builtin_int age;"));
        assert!(result.contains("mvp_builtin_boolean active;"));
    }

    // ===== generate_with_scope: struct with array fields =====

    #[test]
    fn test_cxx_type_array_of_string() {
        let typ = Typ::TArray { of: Box::new(Typ::TString) };
        assert_eq!(cxx_type(&typ), "std::vector<mvp_builtin_string>");
    }

    // ===== Deeply nested struct lit field =====

    #[test]
    fn test_cxx_expr_struct_lit_field_expr() {
        let inner = Expr::EStructLit {
            loc: loc(),
            name: "Inner".into(),
            fields: vec![ValueField {
                name: "val".into(),
                value: Expr::EInt { loc: loc(), value: 5 },
            }],
        };
        let outer = Expr::EStructLit {
            loc: loc(),
            name: "Outer".into(),
            fields: vec![ValueField {
                name: "inner".into(),
                value: inner,
            }],
        };
        let result = cxx_expr(&outer, 0);
        assert!(result.contains("Outer __temp={}"));
        assert!(result.contains("Inner __temp={}"));
        assert!(result.contains("__temp.val = static_cast<mvp_builtin_int>(5)"));
    }

    // ===== Nested blocks =====

    #[test]
    fn test_cxx_expr_nested_block() {
        let inner_block = Expr::EBlock {
            loc: loc(),
            stmts: vec![Stmt::SLet {
                loc: loc(),
                mutable: false,
                name: "y".into(),
                expr: Box::new(Expr::EInt { loc: loc(), value: 2 }),
            }],
            result: Some(Box::new(Expr::EVar { loc: loc(), name: "y".into() })),
        };
        let outer_block = Expr::EBlock {
            loc: loc(),
            stmts: vec![
                Stmt::SLet {
                    loc: loc(),
                    mutable: false,
                    name: "x".into(),
                    expr: Box::new(Expr::EInt { loc: loc(), value: 1 }),
                },
                Stmt::SExpr {
                    loc: loc(),
                    expr: Box::new(inner_block),
                },
            ],
            result: Some(Box::new(Expr::EVoid { loc: loc() })),
        };
        let result = cxx_expr(&outer_block, 0);
        assert!(result.contains("const auto x = static_cast<mvp_builtin_int>(1)"));
        assert!(result.contains("const auto y = static_cast<mvp_builtin_int>(2)"));
    }

    // ===== cxx_main_func with nested modules =====

    #[test]
    fn test_cxx_main_func_with_params() {
        let body = Expr::EBlock {
            loc: loc(),
            stmts: vec![],
            result: None,
        };
        let result = cxx_main_func("".into(), 0, &body);
        assert!(result.contains("mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)"));
        assert!(result.contains("return mvp_builtin_void"));
    }

    // ===== cxx_choose with multiple cases and otherwise =====

    #[test]
    fn test_cxx_choose_multi_case() {
        let e = Expr::EChoose {
            loc: loc(),
            var: Box::new(Expr::EVar { loc: loc(), name: "x".into() }),
            cases: vec![
                WhenCase {
                    when: Box::new(Expr::EInt { loc: loc(), value: 1 }),
                    then: Box::new(Expr::EInt { loc: loc(), value: 10 }),
                },
                WhenCase {
                    when: Box::new(Expr::EInt { loc: loc(), value: 2 }),
                    then: Box::new(Expr::EInt { loc: loc(), value: 20 }),
                },
            ],
            otherwise: Some(Box::new(Expr::EInt { loc: loc(), value: 0 })),
        };
        let result = cxx_expr(&e, 0);
        assert!(result.contains("if (x == static_cast<mvp_builtin_int>(1))"));
        assert!(result.contains("if (x == static_cast<mvp_builtin_int>(2))"));
        assert!(result.contains("else"));
        assert!(result.contains("static_cast<mvp_builtin_int>(0)"));
    }
}
