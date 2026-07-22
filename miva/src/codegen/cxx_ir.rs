use crate::ast::*;
use crate::codegen::cxx::{cxx_param, cxx_type, cxx_func_decl, mangle_cpp_kw, module_parts, cxx_module, cxx_include_here, cxx_include_path, indent_str, cxx_escape_string, map_builtin};
use crate::symbol_table::SymbolTable;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static CLOSURE_DEFS_IR: RefCell<HashMap<usize, IrClosureDef>> = RefCell::new(HashMap::new());
    static CLOSURE_ID_IR: RefCell<usize> = RefCell::new(0);
    static ENUM_DEFS_IR: RefCell<HashMap<String, Vec<crate::ast::EnumVariant>>> =
        RefCell::new(HashMap::new());
}

fn next_closure_id_ir() -> usize {
    CLOSURE_ID_IR.with(|c| {
        let mut id = c.borrow_mut();
        let v = *id;
        *id += 1;
        v
    })
}

fn reset_closure_registry_ir() {
    CLOSURE_DEFS_IR.with(|c| c.borrow_mut().clear());
    CLOSURE_ID_IR.with(|c| *c.borrow_mut() = 0);
}

fn emit_closure_def_ir(def: IrClosureDef) {
    CLOSURE_DEFS_IR.with(|c| { c.borrow_mut().insert(def.id, def); });
}

fn take_closure_defs_ir() -> String {
    let closures: Vec<IrClosureDef> = CLOSURE_DEFS_IR.with(|c| {
        let map = c.borrow();
        let mut ids: Vec<_> = map.keys().copied().collect();
        ids.sort();
        ids.into_iter().map(|id| map[&id].clone()).collect()
    });
    let out: String = closures.iter().map(|cl| {
        let env_name = format!("__closure_env_{}", cl.id);
        let env_fields_str: Vec<_> = cl.env_fields.iter().map(|(n, t)| {
            format!("  {} {};", cxx_type(t), n)
        }).collect();
        let env_struct = if cl.env_fields.is_empty() {
            format!("struct {} {{}};", env_name)
        } else {
            format!("struct {} {{\n{}\n}};", env_name, env_fields_str.join("\n"))
        };
        let ret_cxx = cxx_type(&cl.ret_type);
        let body_str = if cl.thunk_body_stmts.is_empty() {
            format!(
                "{{\n  auto& __env = *static_cast<{}*>(__env_ptr);\n  return {};\n}}",
                env_name,
                emit_expr(cl.thunk_body_result.as_ref().unwrap(), 1, Some(&ret_cxx))
            )
        } else {
            let stmt_strs: String = cl.thunk_body_stmts.iter().map(|s| emit_stmt(s, 2)).collect();
            let capture_bindings: Vec<_> = cl.env_fields.iter().map(|(n, _)| format!("  auto& {} = __env.{};\n", n, n)).collect();
            let bind_str: String = capture_bindings.into_iter().collect();
            let env_cast = format!("  auto& __env = *static_cast<{}*>(__env_ptr);\n", env_name);
            let body = format!("{}{}{}", env_cast, bind_str, stmt_strs);
            match cl.thunk_body_result {
                Some(ref e) => {
                    format!("{{\n{}  return {};\n}}", body, emit_expr(e, 2, Some(&ret_cxx)))
                }
                None => format!("{{\n{}}}", body),
            }
        };
        let thunk = format!(
            "static {} __closure_thunk_{}(void* __env_ptr{}) {}\n",
            ret_cxx, cl.id, if cl.param_list.is_empty() { String::new() } else { format!(", {}", cl.param_list) }, body_str
        );
        format!("{}\n{}", env_struct, thunk)
    }).collect::<Vec<_>>().join("\n\n");
    CLOSURE_DEFS_IR.with(|c| c.borrow_mut().clear());
    out
}

fn record_enum_defs_ir(defs: &[Def]) {
    ENUM_DEFS_IR.with(|m| {
        let mut map = m.borrow_mut();
        map.clear();
        for d in defs {
            if let Def::DEnum { name, variants, .. } = d {
                map.insert(name.clone(), variants.clone());
            }
        }
    });
}

fn first_payload_variant_ir(enum_name: &str) -> Option<String> {
    ENUM_DEFS_IR.with(|m| {
        m.borrow().get(enum_name).and_then(|variants| {
            variants
                .iter()
                .find(|v| !v.payload.is_empty())
                .map(|v| v.name.clone())
        })
    })
}

fn enum_payload_field_ref_ir(var_str: &str, enum_name: &str, variant: &str, field: usize) -> String {
    if first_payload_variant_ir(enum_name).as_deref() == Some(variant) {
        format!("{}.__payload.field{}", var_str, field)
    } else {
        format!("{}.__payload.{}.field{}", var_str, variant, field)
    }
}

fn is_panic_ir(e: &Expr) -> bool {
    match e {
        Expr::ECall { name, .. } if name == "panic" => true,
        Expr::EBlock { stmts, result, .. } => {
            (result.is_none()
                && stmts.iter().any(|s| match s {
                    Stmt::SExpr { expr, .. } => is_panic_ir(expr),
                    Stmt::SReturn { expr, .. } => is_panic_ir(expr),
                    _ => false,
                }))
            || result.as_ref().map_or(false, |r| is_panic_ir(r))
        }
        _ => false,
    }
}

fn is_panic_ir_expr(e: &IrExpr) -> bool {
    match e {
        IrExpr::Call { name, .. } if name == "panic" => true,
        IrExpr::Block { stmts, result, .. } => {
            (result.is_none()
                && stmts.iter().any(|s| match s {
                    IrStmt::Expr(e) => is_panic_ir_expr(e),
                    IrStmt::Return(e) => is_panic_ir_expr(e),
                    _ => false,
                }))
            || result.as_ref().map_or(false, |r| is_panic_ir_expr(r))
        }
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub enum IrExpr {
    Int(i64),
    Bool(bool),
    Float(f64),
    Char(String),
    String(String),
    Void,
    Var(String),
    Move(String),
    Clone(String),
    Call { name: String, type_args: Vec<Typ>, args: Vec<IrExpr> },
    MethodCall { target: Box<IrExpr>, method: String, type_args: Vec<Typ>, args: Vec<IrExpr> },
    BinOp { op: BinOp, left: Box<IrExpr>, right: Box<IrExpr> },
    FieldAccess { expr: Box<IrExpr>, field: String },
    StructInit { name: String, type_args: Vec<Typ>, fields: Vec<(String, IrExpr)> },
    ArrayInit(Vec<IrExpr>),
    Cast { expr: Box<IrExpr>, to: Typ },
    Addr(Box<IrExpr>),
    Deref(Box<IrExpr>),
    IfValue { cond: Box<IrExpr>, then: Box<IrExpr>, else_: Option<Box<IrExpr>>, has_panic: bool },
    Block { stmts: Vec<IrStmt>, result: Option<Box<IrExpr>> },
    While { cond: Box<IrExpr>, body: Vec<IrStmt>, result: Option<Box<IrExpr>> },
    Loop { body: Vec<IrStmt>, result: Option<Box<IrExpr>> },
    For { var: String, range: Box<IrExpr>, body: Vec<IrStmt>, result: Option<Box<IrExpr>> },
    ClosureRef { id: usize },
    Choose { var: Box<IrExpr>, cases: Vec<IrCase>, otherwise: Option<Box<IrExpr>>, has_panic: bool },
    Macro { name: String, args: Vec<IrExpr> },
}

#[derive(Debug, Clone)]
pub enum IrStmt {
    Let { mutable: bool, name: String, expr: IrExpr },
    LetTyped { name: String, typ: Typ, expr: IrExpr },
    Return(IrExpr),
    Expr(IrExpr),
    Assign { name: String, expr: IrExpr },
    FieldAssign { target: IrExpr, field: String, expr: IrExpr },
    Empty,
    If { cond: IrExpr, then: Vec<IrStmt>, else_: Vec<IrStmt> },
    While { cond: IrExpr, body: Vec<IrStmt> },
    Loop { body: Vec<IrStmt> },
    For { var: String, range: IrExpr, body: Vec<IrStmt> },
}

#[derive(Debug, Clone)]
pub struct IrCase {
    pub pattern: IrPattern,
    pub guard: Option<IrExpr>,
    pub then: IrExpr,
}

#[derive(Debug, Clone)]
pub enum IrPattern {
    EnumTag { enum_name: String, variant: String, bindings: Vec<String> },
    Value(IrExpr),
}

#[derive(Debug, Clone)]
pub enum IrDef {
    Struct { name: String, type_params: Vec<String>, fields: Vec<FieldDef> },
    Enum { name: String, type_params: Vec<String>, variants: Vec<EnumVariant> },
    Func { name: String, type_params: Vec<String>, params: Vec<Param>, returns: Option<Typ>, body_stmts: Vec<IrStmt>, body_result: Option<IrExpr>, is_async: bool },
    AsyncFunc { name: String, type_params: Vec<String>, params: Vec<Param>, returns: Option<Typ>, body_stmts: Vec<IrStmt>, body_result: Option<IrExpr> },
    CFunc { name: String, params: Vec<Param>, returns: Option<Typ>, code: String },
    Test { name: String, body_stmts: Vec<IrStmt>, body_result: Option<IrExpr> },
    Impl { struct_name: String, impls: Vec<ImplExpr> },
    Module { name: String, defs: Vec<IrDef> },
    Export(String),
    Import { path: String },
    ImportAs { path: String, alias: String },
    ImportHere { path: String },
    CMagical { content: String },
    CIntro { content: String },
}

#[derive(Debug, Clone)]
pub struct IrClosureDef {
    pub id: usize,
    pub env_fields: Vec<(String, Typ)>,
    pub thunk_sig: String,
    pub param_list: String,
    pub param_types: Vec<String>,
    pub thunk_body_stmts: Vec<IrStmt>,
    pub thunk_body_result: Option<IrExpr>,
    pub ret_type: Typ,
}

pub struct IrContext {
    pub closures: Vec<IrClosureDef>,
    next_id: usize,
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            closures: Vec::new(),
            next_id: 0,
        }
    }

    fn next_id(&mut self) -> usize {
        let v = self.next_id;
        self.next_id += 1;
        v
    }
}

// ===== LOWERING: AST → IR =====

fn lower_expr(ctx: &mut IrContext, expr: &Expr) -> IrExpr {
    match expr {
        Expr::EInt { value, .. } => IrExpr::Int(*value),
        Expr::EBool { value, .. } => IrExpr::Bool(*value),
        Expr::EFloat { value, .. } => IrExpr::Float(*value),
        Expr::EChar { value, .. } => IrExpr::Char(value.clone()),
        Expr::EString { value, .. } => IrExpr::String(value.clone()),
        Expr::EVoid { .. } => IrExpr::Void,
        Expr::EVar { name, .. } => IrExpr::Var(name.clone()),
        Expr::EMove { name, .. } => IrExpr::Move(name.clone()),
        Expr::EClone { name, .. } => IrExpr::Clone(name.clone()),
        Expr::EStructLit { name, fields, type_args, .. } => {
            let lowered_fields: Vec<_> = fields.iter().map(|f| {
                (f.name.clone(), lower_expr(ctx, &f.value))
            }).collect();
            IrExpr::StructInit { name: name.clone(), type_args: type_args.clone(), fields: lowered_fields }
        }
        Expr::EFieldAccess { expr, field, .. } => {
            let lowered_expr = lower_expr(ctx, expr);
            IrExpr::FieldAccess { expr: Box::new(lowered_expr), field: field.clone() }
        }
        Expr::EBinOp { op, left, right, .. } => {
            IrExpr::BinOp { op: op.clone(), left: Box::new(lower_expr(ctx, left)), right: Box::new(lower_expr(ctx, right)) }
        }
        Expr::EIf { cond, then, else_, .. } => {
            let has_panic = else_.as_ref().map_or(false, |e| is_panic_ir(e))
                || is_panic_ir(then);
            IrExpr::IfValue {
                cond: Box::new(lower_expr(ctx, cond)),
                then: Box::new(lower_expr(ctx, then)),
                else_: else_.as_ref().map(|e| Box::new(lower_expr(ctx, e))),
                has_panic,
            }
        }
        Expr::EWhile { cond, body, .. } => {
            let (body_stmts, body_result) = lower_block(ctx, body);
            IrExpr::While {
                cond: Box::new(lower_expr(ctx, cond)),
                body: body_stmts,
                result: body_result.map(Box::new),
            }
        }
        Expr::ELoop { body, .. } => {
            let (body_stmts, body_result) = lower_block(ctx, body);
            IrExpr::Loop {
                body: body_stmts,
                result: body_result.map(Box::new),
            }
        }
        Expr::EFor { var, range, body, .. } => {
            let (body_stmts, body_result) = lower_block(ctx, body);
            IrExpr::For {
                var: var.clone(),
                range: Box::new(lower_expr(ctx, range)),
                body: body_stmts,
                result: body_result.map(Box::new),
            }
        }
        Expr::ECall { name, type_args, args, .. } => {
            let lowered_args: Vec<_> = args.iter().map(|a| lower_expr(ctx, a)).collect();
            IrExpr::Call { name: name.clone(), type_args: type_args.clone(), args: lowered_args }
        }
        Expr::ECast { expr, to, .. } => {
            IrExpr::Cast { expr: Box::new(lower_expr(ctx, expr)), to: to.clone() }
        }
        Expr::EBlock { stmts, result, .. } => {
            if result.is_some() {
                let lowered_stmts: Vec<IrStmt> = stmts.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                IrExpr::Block { stmts: lowered_stmts, result: result.as_ref().map(|e| Box::new(lower_expr(ctx, e))) }
            } else if stmts.last().map_or(false, |s| matches!(s, Stmt::SExpr { .. })) {
                let (non_last, last) = stmts.split_at(stmts.len() - 1);
                let lowered_stmts: Vec<IrStmt> = non_last.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                if let Stmt::SExpr { expr, .. } = &last[0] {
                    IrExpr::Block { stmts: lowered_stmts, result: Some(Box::new(lower_expr(ctx, expr))) }
                } else {
                    unreachable!()
                }
            } else {
                let lowered_stmts: Vec<IrStmt> = stmts.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                IrExpr::Block { stmts: lowered_stmts, result: None }
            }
        }
        Expr::EArrayLit { values, .. } => {
            let lowered: Vec<_> = values.iter().map(|v| lower_expr(ctx, v)).collect();
            IrExpr::ArrayInit(lowered)
        }
        Expr::EAddr { expr, .. } => {
            IrExpr::Addr(Box::new(lower_expr(ctx, expr)))
        }
        Expr::EDeref { expr, .. } => {
            IrExpr::Deref(Box::new(lower_expr(ctx, expr)))
        }
        Expr::EMacro { name, args, .. } => {
            let lowered_args: Vec<_> = args.iter().map(|a| lower_expr(ctx, a)).collect();
            IrExpr::Macro { name: name.clone(), args: lowered_args }
        }
        Expr::EMacroVar { .. } => unreachable!(),
        Expr::EMethodCall { .. } => unreachable!(),
        Expr::EEnumPattern { .. } => {
            unreachable!("EEnumPattern is handled inline in the EChoose arm")
        }
        Expr::ELambda { params, ret, captures, body, .. } => {
            let id = ctx.next_id();
            let ret_cxx = cxx_type(ret);
            let param_strs: Vec<_> = params.iter().map(cxx_param).collect();
            let param_list = param_strs.join(", ");

            let env_fields: Vec<_> = captures
                .iter()
                .map(|(n, t)| (n.clone(), t.clone()))
                .collect();

            let (body_stmts, body_result) = lower_block(ctx, body);

            let inner_tys: Vec<_> = params.iter().map(|p| match p {
                Param::PRef { typ, .. } | Param::POwn { typ, .. } => cxx_type(typ),
            }).collect();
            let closure_ty = if inner_tys.is_empty() {
                format!("mvp_closure<{}>", ret_cxx)
            } else {
                format!("mvp_closure<{}, {}>", ret_cxx, inner_tys.join(", "))
            };

            emit_closure_def_ir(IrClosureDef {
                id,
                env_fields,
                thunk_sig: format!("static {} __closure_thunk_{}(void* __env_ptr, {})", ret_cxx, id, param_list),
                param_list,
                param_types: inner_tys,
                thunk_body_stmts: body_stmts,
                thunk_body_result: body_result,
                ret_type: ret.clone(),
            });

            IrExpr::ClosureRef { id }
        }
        Expr::EChoose { var, cases, otherwise, .. } => {
            let has_panic = cases.iter().any(|c| is_panic_ir(&c.then))
                || otherwise.as_ref().map_or(false, |e| is_panic_ir(e));
            let lowered_var = lower_expr(ctx, var);
            let lowered_cases: Vec<IrCase> = cases.iter().map(|c| {
                let pattern = match c.when.as_ref() {
                    Expr::EEnumPattern { enum_name, variant, bindings, .. } => {
                        IrPattern::EnumTag {
                            enum_name: enum_name.clone(),
                            variant: variant.clone(),
                            bindings: bindings.clone(),
                        }
                    }
                    Expr::EFieldAccess { expr: inner_expr, field: variant, .. } => {
                        if let Expr::EVar { name: enum_name, .. } = inner_expr.as_ref() {
                            if enum_name.chars().next().map_or(false, |c| c.is_uppercase()) {
                                IrPattern::EnumTag {
                                    enum_name: enum_name.clone(),
                                    variant: variant.clone(),
                                    bindings: Vec::new(),
                                }
                            } else {
                                IrPattern::Value(lower_expr(ctx, c.when.as_ref()))
                            }
                        } else {
                            IrPattern::Value(lower_expr(ctx, c.when.as_ref()))
                        }
                    }
                    _ => IrPattern::Value(lower_expr(ctx, c.when.as_ref())),
                };
                let guard = c.guard.as_ref().map(|g| lower_expr(ctx, g));
                IrCase {
                    pattern,
                    guard,
                    then: lower_expr(ctx, c.then.as_ref()),
                }
            }).collect();
            let lowered_otherwise = otherwise.as_ref().map(|e| Box::new(lower_expr(ctx, e)));
            IrExpr::Choose {
                var: Box::new(lowered_var),
                cases: lowered_cases,
                otherwise: lowered_otherwise,
                has_panic,
            }
        }
    }
}

fn lower_block(ctx: &mut IrContext, expr: &Expr) -> (Vec<IrStmt>, Option<IrExpr>) {
    match expr {
        Expr::EBlock { stmts, result, .. } => {
            if result.is_some() {
                let lowered_stmts: Vec<IrStmt> = stmts.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                (lowered_stmts, result.as_ref().map(|e| lower_expr(ctx, e)))
            } else if stmts.last().map_or(false, |s| matches!(s, Stmt::SExpr { .. })) {
                let (non_last, last) = stmts.split_at(stmts.len() - 1);
                let lowered_stmts: Vec<IrStmt> = non_last.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                if let Stmt::SExpr { expr, .. } = &last[0] {
                    (lowered_stmts, Some(lower_expr(ctx, expr)))
                } else {
                    unreachable!()
                }
            } else {
                let lowered_stmts: Vec<IrStmt> = stmts.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                (lowered_stmts, None)
            }
        }
        _ => (Vec::new(), Some(lower_expr(ctx, expr))),
    }
}

fn lower_stmt(ctx: &mut IrContext, stmt: &Stmt) -> Vec<IrStmt> {
    match stmt {
        Stmt::SLet { mutable, name, expr, .. } => {
            vec![IrStmt::Let { mutable: *mutable, name: name.clone(), expr: lower_expr(ctx, expr) }]
        }
        Stmt::SLetTyped { name, typ, expr, .. } => {
            vec![IrStmt::LetTyped { name: name.clone(), typ: typ.clone(), expr: lower_expr(ctx, expr) }]
        }
        Stmt::SReturn { expr, .. } => {
            vec![IrStmt::Return(lower_expr(ctx, expr))]
        }
        Stmt::SExpr { expr, .. } => {
            match expr.as_ref() {
                Expr::EWhile { cond, body, .. } => {
                    let (body_stmts, _) = lower_block(ctx, body);
                    vec![IrStmt::While {
                        cond: lower_expr(ctx, cond),
                        body: body_stmts,
                    }]
                }
                Expr::ELoop { body, .. } => {
                    let (body_stmts, _) = lower_block(ctx, body);
                    vec![IrStmt::Loop { body: body_stmts }]
                }
                Expr::EFor { var, range, body, .. } => {
                    let (body_stmts, _) = lower_block(ctx, body);
                    vec![IrStmt::For {
                        var: var.clone(),
                        range: lower_expr(ctx, range),
                        body: body_stmts,
                    }]
                }
                Expr::EBlock { stmts, result, .. } => {
                    let lowered_stmts: Vec<IrStmt> = stmts.iter().flat_map(|s| lower_stmt(ctx, s)).collect();
                    let lowered_result = result.as_ref().map(|e| Box::new(lower_expr(ctx, e)));
                    vec![IrStmt::Expr(IrExpr::Block { stmts: lowered_stmts, result: lowered_result })]
                }
                _ => vec![IrStmt::Expr(lower_expr(ctx, expr))],
            }
        }
        Stmt::SAssign { name, expr, .. } => {
            vec![IrStmt::Assign { name: name.clone(), expr: lower_expr(ctx, expr) }]
        }
        Stmt::SFieldAssign { target, field, expr, .. } => {
            vec![IrStmt::FieldAssign {
                target: lower_expr(ctx, target),
                field: field.clone(),
                expr: lower_expr(ctx, expr),
            }]
        }
        Stmt::SCIntro { .. } => vec![IrStmt::Empty],
        Stmt::SEmpty { .. } => vec![IrStmt::Empty],
    }
}

fn lower_def(ctx: &mut IrContext, def: &Def) -> IrDef {
    match def {
        Def::DStruct { name, fields, type_params, .. } => {
            IrDef::Struct { name: name.clone(), type_params: type_params.clone(), fields: fields.clone() }
        }
        Def::DEnum { name, variants, type_params, .. } => {
            IrDef::Enum { name: name.clone(), type_params: type_params.clone(), variants: variants.clone() }
        }
        Def::DFunc { name, type_params, params, returns, body, is_async, .. } if *is_async => {
            let (body_stmts, body_result) = lower_block(ctx, body);
            IrDef::AsyncFunc {
                name: name.clone(),
                type_params: type_params.clone(),
                params: params.clone(),
                returns: returns.clone(),
                body_stmts,
                body_result,
            }
        }
        Def::DFunc { name, type_params, params, returns, body, .. } => {
            let (body_stmts, body_result) = lower_block(ctx, body);
            IrDef::Func {
                name: name.clone(),
                type_params: type_params.clone(),
                params: params.clone(),
                returns: returns.clone(),
                body_stmts,
                body_result,
                is_async: false,
            }
        }
        Def::DCFuncUnsafe { name, params, returns, code, .. } => {
            IrDef::CFunc { name: name.clone(), params: params.clone(), returns: returns.clone(), code: code.clone() }
        }
        Def::DTest { name, body, .. } => {
            let (body_stmts, body_result) = lower_block(ctx, body);
            IrDef::Test { name: name.clone(), body_stmts, body_result }
        }
        Def::DImpl { struct_name, impls, .. } => {
            IrDef::Impl { struct_name: struct_name.clone(), impls: impls.clone() }
        }
        Def::DMacro { .. } => unreachable!(),
        Def::DModule { .. } => unreachable!("DModule should be handled by lower_defs"),
        Def::SExport { symbol, .. } => IrDef::Export(symbol.clone()),
        Def::SImport { path, .. } => IrDef::Import { path: path.clone() },
        Def::SImportAs { path, alias, .. } => IrDef::ImportAs { path: path.clone(), alias: alias.clone() },
        Def::SImportHere { path, .. } => IrDef::ImportHere { path: path.clone() },
        Def::DCMagical { content, .. } => IrDef::CMagical { content: content.clone() },
        Def::DCIntro { content, .. } => IrDef::CIntro { content: content.clone() },
    }
}

fn lower_defs(ctx: &mut IrContext, defs: &[Def]) -> Vec<IrDef> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < defs.len() {
        match &defs[i] {
            Def::DModule { name, .. } => {
                let mut inner_ctx = IrContext::new();
                let inner_defs = lower_defs(&mut inner_ctx, &defs[i + 1..]);
                out.push(IrDef::Module { name: name.clone(), defs: inner_defs });
                break;
            }
            _ => {
                out.push(lower_def(ctx, &defs[i]));
                i += 1;
            }
        }
    }
    out
}

// ===== OPTIMIZATION PASSES =====

pub fn optimize_expr(expr: IrExpr) -> IrExpr {
    match expr {
        IrExpr::BinOp { op, left, right } => {
            let l = optimize_expr(*left);
            let r = optimize_expr(*right);
            match (&l, &r) {
                (IrExpr::Int(a), IrExpr::Int(b)) => match op {
                    BinOp::Add => IrExpr::Int(a + b),
                    BinOp::Sub => IrExpr::Int(a - b),
                    BinOp::Mul => IrExpr::Int(a * b),
                    BinOp::Div => IrExpr::Int(a / b),
                    _ => IrExpr::BinOp { op, left: Box::new(IrExpr::Int(*a)), right: Box::new(IrExpr::Int(*b)) },
                },
                (IrExpr::Bool(a), IrExpr::Bool(b)) => match op {
                    BinOp::And => IrExpr::Bool(*a && *b),
                    BinOp::Or => IrExpr::Bool(*a || *b),
                    BinOp::Eq => IrExpr::Bool(*a == *b),
                    BinOp::Neq => IrExpr::Bool(*a != *b),
                    _ => IrExpr::BinOp { op, left: Box::new(IrExpr::Bool(*a)), right: Box::new(IrExpr::Bool(*b)) },
                },
                _ => IrExpr::BinOp { op, left: Box::new(l), right: Box::new(r) },
            }
        }
        IrExpr::IfValue { cond, then, else_, has_panic } => {
            let c = optimize_expr(*cond);
            match c {
                IrExpr::Bool(true) => optimize_expr(*then),
                IrExpr::Bool(false) => {
                    if let Some(e) = else_ { optimize_expr(*e) } else { IrExpr::Void }
                }
                _ => IrExpr::IfValue { cond: Box::new(c), then: optimize_expr_box(then), else_: else_.map(|e| optimize_expr_box(e)), has_panic },
            }
        }
        IrExpr::Block { stmts, result } => {
            let opt_stmts: Vec<IrStmt> = stmts.into_iter().flat_map(optimize_stmt).collect();
            let opt_result = result.map(|e| Box::new(optimize_expr(*e)));
            IrExpr::Block { stmts: opt_stmts, result: opt_result }
        }
        IrExpr::While { cond, body, result } => {
            IrExpr::While {
                cond: Box::new(optimize_expr(*cond)),
                body: body.into_iter().flat_map(optimize_stmt).collect(),
                result: result.map(|e| Box::new(optimize_expr(*e))),
            }
        }
        IrExpr::Loop { body, result } => {
            IrExpr::Loop {
                body: body.into_iter().flat_map(optimize_stmt).collect(),
                result: result.map(|e| Box::new(optimize_expr(*e))),
            }
        }
        IrExpr::For { var, range, body, result } => {
            IrExpr::For {
                var,
                range: Box::new(optimize_expr(*range)),
                body: body.into_iter().flat_map(optimize_stmt).collect(),
                result: result.map(|e| Box::new(optimize_expr(*e))),
            }
        }
        IrExpr::Choose { var, cases, otherwise, has_panic } => {
            IrExpr::Choose {
                var: Box::new(optimize_expr(*var)),
                cases: cases.into_iter().map(|c| IrCase {
                    pattern: c.pattern,
                    guard: c.guard.map(|g| optimize_expr(g)),
                    then: optimize_expr(c.then),
                }).collect(),
                otherwise: otherwise.map(|e| Box::new(optimize_expr(*e))),
                has_panic,
            }
        }
        IrExpr::Cast { expr, to } => {
            let e = optimize_expr(*expr);
            // Fold cast of literal
            match (&e, &to) {
                (IrExpr::Int(v), Typ::TInt) => IrExpr::Int(*v),
                (IrExpr::Float(v), Typ::TFloat64 | Typ::TFloat32) => IrExpr::Float(*v),
                _ => IrExpr::Cast { expr: Box::new(e), to },
            }
        }
        IrExpr::Addr(e) => IrExpr::Addr(Box::new(optimize_expr(*e))),
        IrExpr::Deref(e) => IrExpr::Deref(Box::new(optimize_expr(*e))),
        IrExpr::FieldAccess { expr, field } => {
            IrExpr::FieldAccess { expr: Box::new(optimize_expr(*expr)), field }
        }
        IrExpr::Call { name, type_args, args } => {
            IrExpr::Call { name, type_args, args: args.into_iter().map(optimize_expr).collect() }
        }
        IrExpr::ArrayInit(values) => {
            IrExpr::ArrayInit(values.into_iter().map(optimize_expr).collect())
        }
        IrExpr::StructInit { name, type_args, fields } => {
            IrExpr::StructInit { name, type_args, fields: fields.into_iter().map(|(n, e)| (n, optimize_expr(e))).collect() }
        }
        IrExpr::Macro { name, args } => {
            IrExpr::Macro { name, args: args.into_iter().map(optimize_expr).collect() }
        }
        other => other,
    }
}

fn optimize_expr_box(e: Box<IrExpr>) -> Box<IrExpr> {
    Box::new(optimize_expr(*e))
}

fn flatten_block(stmts: Vec<IrStmt>, result: Option<Box<IrExpr>>) -> (Vec<IrStmt>, Option<Box<IrExpr>>) {
    let mut flat = Vec::new();
    for s in stmts {
        match s {
            IrStmt::Expr(IrExpr::Block { stmts: inner_stmts, result: inner_result }) => {
                let (inner_flat, inner_res) = flatten_block(inner_stmts, inner_result);
                flat.extend(inner_flat);
                if let Some(r) = inner_res {
                    flat.push(IrStmt::Return(*r));
                }
            }
            IrStmt::Return(IrExpr::Block { stmts: inner_stmts, result: inner_result }) => {
                let (inner_flat, inner_res) = flatten_block(inner_stmts, inner_result);
                flat.extend(inner_flat);
                if let Some(r) = inner_res {
                    flat.push(IrStmt::Return(*r));
                }
            }
            other => flat.extend(optimize_stmt(other)),
        }
    }
    (flat, result)
}

pub fn optimize_stmt(stmt: IrStmt) -> Vec<IrStmt> {
    match stmt {
        IrStmt::Let { mutable, name, expr } => {
            vec![IrStmt::Let { mutable, name, expr: optimize_expr(expr) }]
        }
        IrStmt::LetTyped { name, typ, expr } => {
            vec![IrStmt::LetTyped { name, typ, expr: optimize_expr(expr) }]
        }
        IrStmt::Return(e) => vec![IrStmt::Return(optimize_expr(e))],
        IrStmt::Expr(e) => {
            let opt = optimize_expr(e);
            if is_noop_expr(&opt) { vec![] } else { vec![IrStmt::Expr(opt)] }
        }
        IrStmt::Assign { name, expr } => {
            vec![IrStmt::Assign { name, expr: optimize_expr(expr) }]
        }
        IrStmt::FieldAssign { target, field, expr } => {
            vec![IrStmt::FieldAssign { target: optimize_expr(target), field, expr: optimize_expr(expr) }]
        }
        IrStmt::If { cond, then, else_ } => {
            let c = optimize_expr(cond);
            match c {
                IrExpr::Bool(true) => {
                    then.into_iter().flat_map(optimize_stmt).filter(|s| !matches!(s, IrStmt::Empty)).collect()
                }
                IrExpr::Bool(false) => {
                    else_.into_iter().flat_map(optimize_stmt).collect()
                }
                _ => vec![IrStmt::If {
                    cond: c,
                    then: then.into_iter().flat_map(optimize_stmt).collect(),
                    else_: else_.into_iter().flat_map(optimize_stmt).collect(),
                }],
            }
        }
        IrStmt::While { cond, body } => {
            vec![IrStmt::While { cond: optimize_expr(cond), body: body.into_iter().flat_map(optimize_stmt).collect() }]
        }
        IrStmt::Loop { body } => {
            vec![IrStmt::Loop { body: body.into_iter().flat_map(optimize_stmt).collect() }]
        }
        IrStmt::For { var, range, body } => {
            vec![IrStmt::For { var, range: optimize_expr(range), body: body.into_iter().flat_map(optimize_stmt).collect() }]
        }
        IrStmt::Empty => vec![],
    }
}

fn is_noop_expr(e: &IrExpr) -> bool {
    matches!(e, IrExpr::Void)
}

pub fn optimize_def(def: IrDef) -> IrDef {
    match def {
        IrDef::Func { name, type_params, params, returns, body_stmts, body_result, is_async } => {
            let opt_stmts: Vec<IrStmt> = body_stmts.into_iter().flat_map(optimize_stmt).collect();
            let opt_result = body_result.map(optimize_expr);
            IrDef::Func { name, type_params, params, returns, body_stmts: opt_stmts, body_result: opt_result, is_async }
        }
        IrDef::AsyncFunc { name, type_params, params, returns, body_stmts, body_result } => {
            let opt_stmts: Vec<IrStmt> = body_stmts.into_iter().flat_map(optimize_stmt).collect();
            let opt_result = body_result.map(optimize_expr);
            IrDef::AsyncFunc { name, type_params, params, returns, body_stmts: opt_stmts, body_result: opt_result }
        }
        IrDef::Test { name, body_stmts, body_result } => {
            let opt_stmts: Vec<IrStmt> = body_stmts.into_iter().flat_map(optimize_stmt).collect();
            let opt_result = body_result.map(optimize_expr);
            IrDef::Test { name, body_stmts: opt_stmts, body_result: opt_result }
        }
        IrDef::Module { name, defs } => {
            IrDef::Module { name, defs: defs.into_iter().map(optimize_def).collect() }
        }
        other => other,
    }
}

pub fn optimize_defs(defs: Vec<IrDef>) -> Vec<IrDef> {
    defs.into_iter().map(optimize_def).collect()
}

// ===== EMITTER: IR → C++ source =====

pub fn emit_expr(expr: &IrExpr, depth: usize, expected_type: Option<&str>) -> String {
    match expr {
        IrExpr::Int(value) => format!("static_cast<mvp_builtin_int>({})", value),
        IrExpr::Bool(value) => format!(
            "mvp_builtin_boolean({})",
            if *value { "true" } else { "false" }
        ),
        IrExpr::Float(value) => format!("mvp_builtin_float({})", value.to_string()),
        IrExpr::Char(value) => format!("mvp_builtin_byte('{}')", cxx_escape_string(value)),
        IrExpr::String(value) => {
            format!("mvp_builtin_string(\"{}\")", cxx_escape_string(value))
        }
        IrExpr::Var(name) => mangle_cpp_kw(name),
        IrExpr::Move(name) => format!("std::move({})", mangle_cpp_kw(name)),
        IrExpr::Clone(name) => format!("decltype({})({})", mangle_cpp_kw(name), mangle_cpp_kw(name)),
        IrExpr::Void => "mvp_builtin_void".into(),
        IrExpr::Call { name, type_args, args } => emit_call(name, type_args, args, depth, expected_type),
        IrExpr::MethodCall { target, method, type_args, args } => {
            unreachable!("EMethodCall should not reach emitter")
        }
        IrExpr::BinOp { op, left, right } => emit_binop(op, left, right, depth, expected_type),
        IrExpr::FieldAccess { expr, field } => emit_field_access(expr, field, depth, expected_type),
        IrExpr::StructInit { name, type_args, fields } => emit_struct_lit(name, type_args, fields, depth, expected_type),
        IrExpr::ArrayInit(values) => {
            let elems: Vec<_> = values.iter().map(|e| emit_expr(e, depth, expected_type)).collect();
            format!("std::vector{{{}}}", elems.join(", "))
        }
        IrExpr::Cast { expr, to } => {
            let from_int = matches!(expr.as_ref(), IrExpr::Int { .. }) || matches!(expr.as_ref(), IrExpr::Var { .. });
            let is_ptr_cast = matches!(to, Typ::TPtrAny) || from_int && matches!(to, Typ::TPtrAny);
            if is_ptr_cast {
                format!("reinterpret_cast<{}>({})", cxx_type(to), emit_expr(expr, depth, expected_type))
            } else {
                format!("static_cast<{}>({})", cxx_type(to), emit_expr(expr, depth, expected_type))
            }
        }
        IrExpr::Addr(expr) => format!("&({})", emit_expr(expr, depth, expected_type)),
        IrExpr::Deref(expr) => format!("*({})", emit_expr(expr, depth, expected_type)),
        IrExpr::IfValue { cond, then, else_, has_panic } => emit_if(cond, then, else_, *has_panic, depth, expected_type),
        IrExpr::Block { stmts, result } => emit_block(stmts, result, depth, expected_type),
        IrExpr::While { cond, body, result } => emit_while(cond, body, result, depth, expected_type),
        IrExpr::Loop { body, result } => emit_loop(body, result, depth, expected_type),
        IrExpr::For { var, range, body, result } => emit_for(var, range, body, result, depth, expected_type),
        IrExpr::ClosureRef { id } => emit_closure_ref(*id),
        IrExpr::Choose { var, cases, otherwise, has_panic } => emit_choose(var, cases, otherwise, *has_panic, depth, expected_type),
        IrExpr::Macro { .. } => String::new(),
    }
}

fn emit_call(name: &str, type_args: &[Typ], args: &[IrExpr], depth: usize, expected_type: Option<&str>) -> String {
    let args_strs: Vec<_> = args.iter().map(|a| emit_expr(a, depth, expected_type)).collect();
    let type_arg_str = if type_args.is_empty() {
        String::new()
    } else {
        let tas: Vec<_> = type_args.iter().map(cxx_type).collect();
        format!("<{}>", tas.join(", "))
    };
    if name.matches('.').count() == 1 {
        let dot = name.find('.').unwrap();
        let enum_name = &name[..dot];
        let variant = &name[dot + 1..];
        return format!("{}_{}{}({})", enum_name, variant, type_arg_str, args_strs.join(", "));
    } else if let Some(enum_name) = args.first().and_then(|a| match a {
        IrExpr::Var(n) => Some(n.as_str()),
        _ => None,
    }) {
        if enum_name.starts_with(|c: char| c.is_uppercase()) {
            let payload_strs = &args_strs[1..];
            return format!("{}_{}{}({})", enum_name, name, type_arg_str, payload_strs.join(", "));
        }
    }
    format!(
        "{}{}({})",
        map_builtin(name),
        type_arg_str,
        args_strs.join(", ")
    )
}

fn emit_binop(op: &BinOp, left: &IrExpr, right: &IrExpr, depth: usize, expected_type: Option<&str>) -> String {
    let op_str = match op {
        BinOp::Add => " + ",
        BinOp::Sub => " - ",
        BinOp::Mul => " * ",
        BinOp::Div => " / ",
        BinOp::Eq => " == ",
        BinOp::Neq => " != ",
        BinOp::Lt => " < ",
        BinOp::Gt => " > ",
        BinOp::Le => " <= ",
        BinOp::Ge => " >= ",
        BinOp::And => " && ",
        BinOp::Or => " || ",
    };
    format!(
        "({}{}{})",
        emit_expr(left, depth, expected_type),
        op_str,
        emit_expr(right, depth, expected_type)
    )
}

fn emit_field_access(expr: &IrExpr, field: &str, depth: usize, expected_type: Option<&str>) -> String {
    if field.chars().all(|c| c.is_ascii_digit()) {
        format!("{}.__payload.field{}", emit_expr(expr, depth, expected_type), field)
    } else if let IrExpr::Var(enum_name) = expr {
        if enum_name.chars().next().map_or(false, |c| c.is_uppercase()) {
            format!("{}_{}()", enum_name, field)
        } else {
            format!("{}.{}", emit_expr(expr, depth, expected_type), field)
        }
    } else {
        format!("{}.{}", emit_expr(expr, depth, expected_type), field)
    }
}

fn emit_struct_lit(name: &str, type_args: &[Typ], fields: &[(String, IrExpr)], depth: usize, expected_type: Option<&str>) -> String {
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
            .map(|f| format!("{}.{} = {}", temp, f.0, emit_expr(&f.1, depth + 1, expected_type)))
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

fn emit_if(cond: &IrExpr, then: &IrExpr, else_: &Option<Box<IrExpr>>, has_panic: bool, depth: usize, expected_type: Option<&str>) -> String {
    let cond_str = emit_expr(cond, depth, expected_type);
    let then_str = emit_expr(then, depth + 1, expected_type);
    let else_str = match else_ {
        Some(e) => format!(" else {{ {}; }}", emit_expr(e, depth + 1, expected_type)),
        None => String::new(),
    };
    format!(
        "([&]() -> void {{ if ({}) {{ {}; }}{} }})()",
        cond_str, then_str, else_str
    )
}

fn emit_block(stmts: &[IrStmt], result: &Option<Box<IrExpr>>, depth: usize, expected_type: Option<&str>) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    let stmt_strs: String = stmts.iter().fold(String::new(), |acc, stmt| {
        format!("{}{}", acc, emit_stmt(stmt, inner))
    });
    let result_str = match result {
        Some(expr) => format!("{}return {};\n", indent_str(inner), emit_expr(expr, inner, expected_type)),
        None => String::new(),
    };
    format!("([&]() {{\n{}{}{}}})()", stmt_strs, result_str, ind)
}

fn emit_while(cond: &IrExpr, body: &[IrStmt], result: &Option<Box<IrExpr>>, depth: usize, expected_type: Option<&str>) -> String {
    let cond_str = emit_expr(cond, depth, expected_type);
    let body_str = if body.is_empty() {
        String::new()
    } else {
        let stmt_strs: String = body.iter().map(|s| emit_stmt(s, depth + 1)).collect();
        let result_str = match result {
            Some(e) => format!("{}return {};\n", indent_str(depth + 1), emit_expr(e, depth + 1, expected_type)),
            None => String::new(),
        };
        format!("{{\n{}{}\n{}}}", stmt_strs, result_str, indent_str(depth))
    };
    format!("([&]() {{ while ({}) {{ {} ;}}}})()", cond_str, body_str)
}

fn emit_loop(body: &[IrStmt], result: &Option<Box<IrExpr>>, depth: usize, expected_type: Option<&str>) -> String {
    let body_str = if body.is_empty() {
        String::new()
    } else {
        let stmt_strs: String = body.iter().map(|s| emit_stmt(s, depth + 1)).collect();
        let result_str = match result {
            Some(e) => format!("{}return {};\n", indent_str(depth + 1), emit_expr(e, depth + 1, expected_type)),
            None => String::new(),
        };
        format!("{{\n{}{}\n{}}}", stmt_strs, result_str, indent_str(depth))
    };
    format!("([&]() {{ for (;;) {{ {} ;}}}})()", body_str)
}

fn emit_for(var: &str, range: &IrExpr, body: &[IrStmt], result: &Option<Box<IrExpr>>, depth: usize, expected_type: Option<&str>) -> String {
    let range_str = emit_expr(range, depth, expected_type);
    let body_str = if body.is_empty() {
        String::new()
    } else {
        let stmt_strs: String = body.iter().map(|s| emit_stmt(s, depth + 1)).collect();
        let result_str = match result {
            Some(e) => format!("{}return {};\n", indent_str(depth + 1), emit_expr(e, depth + 1, expected_type)),
            None => String::new(),
        };
        format!("{{\n{}{}\n{}}}", stmt_strs, result_str, indent_str(depth))
    };
    format!(
        "([&]() {{ for (const auto& {} : {}) {{ {} ;}}}})()",
        var, range_str, body_str
    )
}

fn emit_closure_ref(id: usize) -> String {
    let env_name = format!("__closure_env_{}", id);
    let thunk_name = format!("__closure_thunk_{}", id);
    let closure = CLOSURE_DEFS_IR.with(|c| {
        let closures = c.borrow();
        closures.get(&id).map(|cl| cl.clone())
    });
    match closure {
        Some(cl) => {
            let capture_exprs: Vec<_> = cl.env_fields.iter().map(|(n, _)| format!("{},", n)).collect();
            let capture_init = if capture_exprs.is_empty() {
                String::new()
            } else {
                format!("{{{}}}", capture_exprs.join(" "))
            };
            let closure_ty = if cl.param_types.is_empty() {
                let ret_cxx = cxx_type(&cl.ret_type);
                format!("mvp_closure<{}>", ret_cxx)
            } else {
                let ret_cxx = cxx_type(&cl.ret_type);
                format!("mvp_closure<{}, {}>", ret_cxx, cl.param_types.join(", "))
            };
            format!(
                "({} {{ new {}{}, &{}, [](void* p) {{ delete static_cast<{}*>(p); }} }})",
                closure_ty, env_name, capture_init, thunk_name, env_name
            )
        }
        None => format!("/* missing closure {} */", id),
    }
}

fn emit_choose(var: &IrExpr, cases: &[IrCase], otherwise: &Option<Box<IrExpr>>, has_panic: bool, depth: usize, expected_type: Option<&str>) -> String {
    let var_str = emit_expr(var, depth, expected_type);
    let ind = indent_str(depth);
    let inner = depth + 1;
    let ret_suffix = match expected_type {
        Some(t) => format!(" -> {} ", t),
        None => String::new(),
    };
    let phantom_return = |t: &str| -> String { format!("return {}();", t) };
    let cast = |t: &str, e: &str| -> String { format!("static_cast<{}>({})", t, e) };
    let branch_body = |e: &IrExpr| -> String {
        if is_panic_ir_expr(e) {
            if let Some(t) = expected_type {
                format!(
                    "{}{}; {}",
                    indent_str(inner),
                    emit_expr(e, inner, expected_type),
                    phantom_return(t)
                )
            } else {
                format!("{}{};", indent_str(inner), emit_expr(e, inner, expected_type))
            }
        } else {
            let inner_expr = emit_expr(e, inner, expected_type);
            match expected_type {
                Some(t) => format!("{}return {};", indent_str(inner), cast(t, &inner_expr)),
                None => format!("{}return {};", indent_str(inner), inner_expr),
            }
        }
    };
    let cases_str: String = cases.iter().fold(String::new(), |acc, c| {
        let guard_str = match &c.guard {
            Some(g) => format!(" && ({})", emit_expr(g, depth, expected_type)),
            None => String::new(),
        };
        let (tag_disc, binding_names, bind_enum, bind_variant) = match &c.pattern {
            IrPattern::EnumTag { enum_name, variant, bindings } => (
                format!("{}.__tag == {}_{}_tag()", var_str, enum_name, variant),
                bindings.clone(),
                enum_name.clone(),
                variant.clone(),
            ),
            IrPattern::Value(e) => (String::new(), Vec::new(), String::new(), String::new()),
        };
        if !tag_disc.is_empty() {
            let bind_str: String = binding_names
                .iter()
                .enumerate()
                .map(|(i, b)| {
                    format!(
                        "{}{}const auto {} = {};\n",
                        indent_str(inner),
                        ind,
                        b,
                        enum_payload_field_ref_ir(&var_str, &bind_enum, &bind_variant, i)
                    )
                })
                .collect();
            let body = branch_body(&c.then);
            match &c.guard {
                Some(_) => {
                    let guard_expr = emit_expr(c.guard.as_ref().unwrap(), inner, expected_type);
                    format!(
                        "{}{}if ({}) {{\n{}{}if ({}) {{\n{}{}\n{}}}\n{}}}\n",
                        acc,
                        ind,
                        tag_disc,
                        bind_str,
                        indent_str(inner),
                        guard_expr,
                        indent_str(inner),
                        body,
                        ind,
                        ind
                    )
                }
                None => {
                    format!(
                        "{}{}if ({}) {{\n{}{}\n{}}}\n",
                        acc,
                        ind,
                        tag_disc,
                        bind_str,
                        body,
                        ind
                    )
                }
            }
        } else {
            let value_str = emit_expr(match &c.pattern { IrPattern::Value(e) => e, _ => unreachable!() }, depth, expected_type);
            let body = branch_body(&c.then);
            format!(
                "{}{}if (({} == {}){}) {{ {} }}\n",
                acc, ind, var_str, value_str, guard_str, body
            )
        }
    });
    let otherwise_str = match otherwise {
        Some(e) if is_panic_ir_expr(e) => {
            if let Some(t) = expected_type {
                format!(
                    "{}else {{ {}; {} }}",
                    ind,
                    emit_expr(e, inner, expected_type),
                    phantom_return(t)
                )
            } else {
                format!("{}else {{ {}; }}", ind, emit_expr(e, inner, expected_type))
            }
        }
        Some(e) => {
            let inner_expr = emit_expr(e, inner, expected_type);
            match expected_type {
                Some(t) => format!("{}else {{ return {}; }}", ind, cast(t, &inner_expr)),
                None => format!("{}else {{ return {}; }}", ind, inner_expr),
            }
        }
        None => String::new(),
    };
    format!(
        "([&](){} {{\n{}{}{}\n{}\n}}())",
        ret_suffix, cases_str, otherwise_str, ind, ind
    )
}

pub fn emit_stmt(stmt: &IrStmt, depth: usize) -> String {
    let ind = indent_str(depth);
    match stmt {
        IrStmt::Let { mutable, name, expr } => {
            let mut_str = if *mutable { "auto " } else { "const auto " };
            format!("{}{}{} = {};\n", ind, mut_str, name, emit_expr(expr, depth, None))
        }
        IrStmt::LetTyped { name, typ, expr } => {
            format!(
                "{}{} {} = {};\n",
                ind,
                cxx_type(typ),
                name,
                emit_expr(expr, depth, None)
            )
        }
        IrStmt::Return(expr) => {
            format!("{}return {};\n", ind, emit_expr(expr, depth, None))
        }
        IrStmt::Expr(expr) => {
            format!("{}{};\n", ind, emit_expr(expr, depth, None))
        }
        IrStmt::Assign { name, expr } => {
            format!("{}{} = {};\n", ind, name, emit_expr(expr, depth, None))
        }
        IrStmt::FieldAssign { target, field, expr } => {
            format!(
                "{}({}).{} = ({});\n",
                ind,
                emit_expr(target, depth, None),
                field,
                emit_expr(expr, depth, None)
            )
        }
        IrStmt::Empty => String::new(),
        IrStmt::If { cond, then, else_ } => emit_plain_if(cond, then, else_, depth),
        IrStmt::While { cond, body } => emit_plain_while(cond, body, depth),
        IrStmt::Loop { body } => emit_plain_loop(body, depth),
        IrStmt::For { var, range, body } => emit_plain_for(var, range, body, depth),
    }
}

fn emit_plain_if(cond: &IrExpr, then: &[IrStmt], else_: &[IrStmt], depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    let cond_str = emit_expr(cond, depth, None);
    let then_str: String = then.iter().map(|s| emit_stmt(s, inner)).collect();
    let else_str = if else_.is_empty() {
        String::new()
    } else {
        let else_body: String = else_.iter().map(|s| emit_stmt(s, inner)).collect();
        format!("{}else {{\n{}{}}}\n", ind, else_body, ind)
    };
    format!("{}if ({}) {{\n{}{}}}\n", ind, cond_str, then_str, ind)
}

fn emit_plain_while(cond: &IrExpr, body: &[IrStmt], depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    let cond_str = emit_expr(cond, depth, None);
    let body_str: String = body.iter().map(|s| emit_stmt(s, inner)).collect();
    format!("{}while ({}) {{\n{}{}}}\n", ind, cond_str, body_str, ind)
}

fn emit_plain_loop(body: &[IrStmt], depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    let body_str: String = body.iter().map(|s| emit_stmt(s, inner)).collect();
    format!("{}for (;;) {{\n{}{}}}\n", ind, body_str, ind)
}

fn emit_plain_for(var: &str, range: &IrExpr, body: &[IrStmt], depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    let range_str = emit_expr(range, depth, None);
    let body_str: String = body.iter().map(|s| emit_stmt(s, inner)).collect();
    format!("{}for (const auto& {} : {}) {{\n{}{}}}\n", ind, var, range_str, body_str, ind)
}

pub fn emit_def(def: &IrDef, depth: usize) -> String {
    let ind = indent_str(depth);
    let inner = depth + 1;
    match def {
        IrDef::Struct { name, type_params, fields } => emit_struct_def(name, type_params, fields, ind, inner),
        IrDef::Enum { name, type_params, variants } => emit_enum_def(name, type_params, variants, ind, inner),
        IrDef::Func { name, type_params, params, returns, body_stmts, body_result, .. } => emit_normal_func(name, type_params, params, returns, body_stmts, body_result, ind, inner),
        IrDef::AsyncFunc { name, type_params, params, returns, body_stmts, body_result } => emit_async_func(name, type_params, params, returns, body_stmts, body_result, ind, inner),
        IrDef::CFunc { name, params, returns, code } => emit_cfunc(name, params, returns, code, ind),
        IrDef::Test { name, body_stmts, body_result } => emit_test(name, body_stmts, body_result, ind, inner),
        IrDef::Impl { struct_name, impls } => emit_impl(struct_name, impls, ind),
        IrDef::Module { name, defs } => emit_module(name, defs, ind, inner),
        IrDef::Export(symbol) => String::new(),
        IrDef::Import { .. } | IrDef::ImportAs { .. } | IrDef::ImportHere { .. } => String::new(),
        IrDef::CMagical { .. } | IrDef::CIntro { .. } => String::new(),
    }
}

fn emit_struct_def(name: &str, type_params: &[String], fields: &[FieldDef], ind: String, inner: usize) -> String {
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

fn emit_enum_def(name: &str, type_params: &[String], variants: &[crate::ast::EnumVariant], ind: String, inner: usize) -> String {
    let template = if type_params.is_empty() {
        String::new()
    } else {
        format!(
            "template<{}>\n",
            type_params
                .iter()
                .map(|tp| format!("typename {}", tp))
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    let first_payload_idx = variants.iter().position(|v| !v.payload.is_empty());
    let mut payload_members = String::new();
    for (v_idx, v) in variants.iter().enumerate() {
        if v.payload.is_empty() {
            continue;
        }
        let fields: Vec<String> = v
            .payload
            .iter()
            .enumerate()
            .map(|(i, t)| format!("{} field{};", cxx_type(t), i))
            .collect();
        if Some(v_idx) == first_payload_idx {
            payload_members.push_str(&format!(
                "{}{}\n",
                indent_str(inner + 1),
                fields.join(" ")
            ));
        } else {
            payload_members.push_str(&format!(
                "{}struct {{ {} }} {};\n",
                indent_str(inner + 1),
                fields.join(" "),
                v.name
            ));
        }
    }
    let payload_block = if payload_members.is_empty() {
        format!("{}struct {{}} __payload;\n", indent_str(inner))
    } else {
        format!(
            "{}struct {{\n{}{}}} __payload;\n",
            indent_str(inner),
            payload_members,
            indent_str(inner)
        )
    };
    let struct_str = format!(
        "{}{}struct {} {{\n{}mvp_builtin_int __tag;\n{}{}bool operator==(const {}& o) const {{ return __tag == o.__tag; }}\n{}bool operator!=(const {}& o) const {{ return __tag != o.__tag; }}\n{}}};\n\n",
        template, ind, name, indent_str(inner), payload_block, indent_str(inner), name, indent_str(inner), name, ind
    );
    let ret_ty = if type_params.is_empty() {
        name.to_string()
    } else {
        format!("{}<{}>", name, type_params.join(", "))
    };
    let mut ctors = String::new();
    for (idx, v) in variants.iter().enumerate() {
        let params: Vec<String> = v
            .payload
            .iter()
            .enumerate()
            .map(|(i, t)| format!("{} __a{}", cxx_type(t), i))
            .collect();
        let inits: Vec<String> = (0..v.payload.len())
            .map(|i| {
                let field = if Some(idx) == first_payload_idx {
                    format!("field{}", i)
                } else {
                    format!("{}.field{}", v.name, i)
                };
                format!("v.__payload.{} = __a{};", field, i)
            })
            .collect();
        let ctor = format!(
            "{}{}inline {} {}_{}({}) {{\n{} {} v;\n{} v.__tag = {};\n{} {};\n{} return v;\n{}}}\n\n",
            template, ind,
            ret_ty,
            name,
            v.name,
            params.join(", "),
            indent_str(inner),
            ret_ty,
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
            "{}{}inline {} {}_{}() {{\n{} {} v;\n{} v.__tag = {};\n{} return v;\n{}}}\n\n",
            template, ind, ret_ty, name, v.name, indent_str(inner), ret_ty, indent_str(inner), idx, indent_str(inner), ind
        );
        ctors.push_str(&disc);
    }
    let mut tag_fns = String::new();
    for (idx, v) in variants.iter().enumerate() {
        tag_fns.push_str(&format!(
            "{}inline mvp_builtin_int {}_{}_tag() {{ return {}; }}\n\n",
            ind, name, v.name, idx
        ));
    }
    struct_str + &ctors + &tag_fns
}

fn emit_cfunc(name: &str, params: &[Param], returns: &Option<Typ>, code: &str, ind: String) -> String {
    let param_strs: Vec<_> = params.iter().map(cxx_param).collect();
    let ret_type = returns.as_ref().map_or("mvp_builtin_unit".into(), cxx_type);
    let signature = format!("{} {}({})", ret_type, mangle_cpp_kw(name), param_strs.join(", "));
    format!("{} {} {{\n{}{}}}\n\n", ind, signature, code, ind)
}

fn emit_normal_func(name: &str, type_params: &[String], params: &[Param], returns: &Option<Typ>, body_stmts: &[IrStmt], body_result: &Option<IrExpr>, ind: String, _inner: usize) -> String {
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
    let body_str = if ret_type == "mvp_builtin_unit" {
        let stmt_strs: String = body_stmts.iter().map(|s| emit_stmt(s, 1)).collect();
        let ret_line = match body_result {
            Some(expr) => format!("  return {};\n", emit_expr(expr, 1, None)),
            None => "  return mvp_builtin_void;\n".into(),
        };
        format!(
            "{} {} {{\n{}{}{}}}\n\n",
            ind,
            signature,
            stmt_strs,
            ret_line,
            ind
        )
    } else {
        let stmt_strs: String = body_stmts.iter().map(|s| emit_stmt(s, 1)).collect();
        let ret_line = match body_result {
            Some(expr) => format!("  return {};\n", emit_expr(expr, 1, Some(ret_type.as_str()))),
            None => format!("  return {}();\n", ret_type),
        };
        format!(
            "{} {} {{\n{}{}{}}}\n\n",
            ind,
            signature,
            stmt_strs,
            ret_line,
            ind
        )
    };
    format!("{}{}", template_header, body_str)
}

fn emit_async_func(name: &str, type_params: &[String], params: &[Param], returns: &Option<Typ>, body_stmts: &[IrStmt], body_result: &Option<IrExpr>, ind: String, inner: usize) -> String {
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
    let capture_list = if params.is_empty() {
        "[]".to_string()
    } else {
        let names: Vec<_> = params
            .iter()
            .map(|p| match p {
                Param::PRef { name, .. } | Param::POwn { name, .. } => mangle_cpp_kw(name),
            })
            .collect();
        format!("[{}]", names.join(", "))
    };
    let stmt_strs: String = body_stmts.iter().map(|s| emit_stmt(s, inner + 1)).collect();
    let result_str = match body_result {
        Some(expr) => format!("{}return {};\n", indent_str(inner + 1), emit_expr(expr, inner + 1, Some(&inner_ret))),
        None => String::new(),
    };
    let lambda = format!(
        "return mvp_async_spawn({}() -> {} {{\n{}{}}});\n",
        capture_list, inner_ret, stmt_strs, result_str
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

fn emit_test(name: &str, body_stmts: &[IrStmt], body_result: &Option<IrExpr>, ind: String, inner: usize) -> String {
    let signature = format!("mvp_builtin_int {}", name);
    let stmt_strs: String = body_stmts.iter().map(|s| emit_stmt(s, inner)).collect();
    let ret_line = match body_result {
        Some(expr) => format!("{}return {};\n", indent_str(inner), emit_expr(expr, inner, Some("mvp_builtin_int"))),
        None => format!("{}return mvp_builtin_void;\n", indent_str(inner)),
    };
    format!(
        "{} {} {{\n{}{}{}}}\n\n",
        ind, signature, stmt_strs, ret_line, ind
    )
}

fn emit_impl(struct_name: &str, impls: &[ImplExpr], ind: String) -> String {
    let mut ret = String::new();
    for impl_expr in impls {
        let op = &impl_expr.op;
        let fn_name = &impl_expr.func;
        let ret_typ = match op {
            ImplOp::ImAdd | ImplOp::ImSub | ImplOp::ImMul | ImplOp::ImDiv => struct_name.to_string(),
            ImplOp::ImEq | ImplOp::ImNeq => "mvp_builtin_boolean".to_string(),
        };
        let operator = match op {
            ImplOp::ImAdd => "+",
            ImplOp::ImSub => "-",
            ImplOp::ImMul => "*",
            ImplOp::ImDiv => "/",
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

fn emit_module(name: &str, defs: &[IrDef], ind: String, inner: usize) -> String {
    let parts = module_parts(name);
    let ns_start: String = parts
        .iter()
        .map(|p| format!("namespace {} {{\n\n", p))
        .collect();
    let ns_end: String = parts.iter().map(|_| "}\n\n".to_string()).collect();
    let defs_str: String = defs.iter().map(|d| emit_def(d, inner)).collect();
    format!("{}{}{}", ns_start, defs_str, ns_end)
}

// ===== PROGRAM ASSEMBLY =====

struct ScopePartsIr {
    includes: String,
    defs_str: String,
    main_functions: String,
}

fn generate_with_scope_ir(defs: &[IrDef], module: Option<&str>) -> ScopePartsIr {
    let mut includes = String::new();
    let mut defs_str = String::new();
    let mut main_functions = String::new();

    for (i, def) in defs.iter().enumerate() {
        match def {
            IrDef::Module { name, defs: inner_defs, .. } => {
                let parts = module_parts(name);
                let ns_start: String = parts
                    .iter()
                    .map(|p| format!("namespace {} {{\n\n", p))
                    .collect();
                let ns_end: String = parts.iter().map(|_| "}\n\n".to_string()).collect();
                let inner = generate_with_scope_ir(inner_defs, Some(name.as_str()));
                includes.push_str(&inner.includes);
                defs_str.push_str(&ns_start);
                defs_str.push_str(&inner.defs_str);
                defs_str.push_str(&ns_end);
                main_functions.push_str(&inner.main_functions);
                break;
            }
            IrDef::Func { name, body_stmts, body_result, .. } if name == "main" => {
                let mvp_main_str = emit_main_func(body_stmts, body_result);
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
            IrDef::Import { path, .. } => includes.push_str(&cxx_include_here(path)),
            IrDef::ImportAs { path, .. } => includes.push_str(&cxx_include_path(path)),
            IrDef::ImportHere { path, .. } => includes.push_str(&cxx_include_here(path)),
            _ => defs_str.push_str(&emit_def(def, 0)),
        }
    }

    ScopePartsIr {
        includes,
        defs_str,
        main_functions,
    }
}

fn emit_main_func(body_stmts: &[IrStmt], body_result: &Option<IrExpr>) -> String {
    let signature = "mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)";
    let stmt_strs: String = body_stmts
        .iter()
        .map(|s| emit_stmt(s, 1))
        .collect::<Vec<_>>()
        .join("");
    let ret_line = match body_result {
        Some(expr) => format!("  {};\n  return mvp_builtin_void;\n", emit_expr(expr, 1, None)),
        None => "  return mvp_builtin_void;\n".into(),
    };
    let mut out = String::new();
    out.push_str(signature);
    out.push_str(" {\n");
    out.push_str(&stmt_strs);
    out.push_str(&ret_line);
    out.push_str("}\n\n");
    out
}

fn collect_imports_ir(defs: &[IrDef], out: &mut Vec<String>) {
    for d in defs.iter() {
        match d {
            IrDef::Import { path, .. }
            | IrDef::ImportHere { path, .. }
            | IrDef::ImportAs { path, .. } => out.push(path.clone()),
            IrDef::Module { defs: inner, .. } => collect_imports_ir(inner, out),
            _ => {}
        }
    }
}

fn generate_header_ir(defs: &[IrDef]) -> String {
    let sym = SymbolTable::build(&defs_to_ast(defs));
    let mut exported = String::new();
    collect_exported_rec_ir(defs, &sym, &[], &mut exported);
    if exported.is_empty() {
        return String::new();
    }
    let mut includes = String::from("#include <mvp_builtin.h>\n");
    let mut import_paths = Vec::new();
    collect_imports_ir(defs, &mut import_paths);
    for path in &import_paths {
        let inc = cxx_include_path(path);
        if !inc.is_empty() {
            includes.push_str(&inc);
        }
    }
    format!("#pragma once\n\n{}\n{}\n", includes, exported)
}

fn defs_to_ast(defs: &[IrDef]) -> Vec<Def> {
    defs.iter().flat_map(|d| ir_def_to_ast_rec(d)).collect()
}

fn ir_def_to_ast_rec(def: &IrDef) -> Vec<Def> {
    match def {
        IrDef::Module { name, defs: inner_defs } => {
            let mut result = vec![Def::DModule {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
            }];
            for inner in inner_defs {
                result.extend(ir_def_to_ast_rec(inner));
            }
            result
        }
        _ => vec![ir_def_to_ast(def)],
    }
}

fn ir_def_to_ast(def: &IrDef) -> Def {
    match def {
        IrDef::Struct { name, fields, type_params, .. } => {
            Def::DStruct {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
                fields: fields.clone(),
                type_params: type_params.clone(),
            }
        }
        IrDef::Enum { name, variants, type_params, .. } => {
            Def::DEnum {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
                variants: variants.clone(),
                type_params: type_params.clone(),
            }
        }
        IrDef::Func { name, type_params, params, returns, .. } => {
            Def::DFunc {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
                type_params: type_params.clone(),
                params: params.clone(),
                returns: returns.clone(),
                body: Box::new(Expr::EVoid { loc: Loc { line: 1, col: 1 } }),
                safety: Safety::Safe,
                is_async: false,
            }
        }
        IrDef::AsyncFunc { name, type_params, params, returns, .. } => {
            Def::DFunc {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
                type_params: type_params.clone(),
                params: params.clone(),
                returns: returns.clone(),
                body: Box::new(Expr::EVoid { loc: Loc { line: 1, col: 1 } }),
                safety: Safety::Safe,
                is_async: true,
            }
        }
        IrDef::CFunc { name, params, returns, .. } => {
            Def::DCFuncUnsafe {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
                params: params.clone(),
                returns: returns.clone(),
                code: String::new(),
                safety: Safety::Unsafe,
                used_c_keyword: false,
            }
        }
        IrDef::Test { name, .. } => {
            Def::DTest {
                loc: Loc { line: 1, col: 1 },
                name: name.clone(),
                body: Box::new(Expr::EVoid { loc: Loc { line: 1, col: 1 } }),
            }
        }
        IrDef::Impl { struct_name, impls } => {
            Def::DImpl {
                loc: Loc { line: 1, col: 1 },
                struct_name: struct_name.clone(),
                impls: impls.clone(),
            }
        }
        IrDef::Module { name, .. } => Def::DModule {
            loc: Loc { line: 1, col: 1 },
            name: name.clone(),
        },
        IrDef::Export(symbol) => Def::SExport {
            loc: Loc { line: 1, col: 1 },
            symbol: symbol.clone(),
        },
        IrDef::Import { path } => Def::SImport {
            loc: Loc { line: 1, col: 1 },
            path: path.clone(),
        },
        IrDef::ImportAs { path, alias } => Def::SImportAs {
            loc: Loc { line: 1, col: 1 },
            path: path.clone(),
            alias: alias.clone(),
        },
        IrDef::ImportHere { path } => Def::SImportHere {
            loc: Loc { line: 1, col: 1 },
            path: path.clone(),
        },
        IrDef::CMagical { content } => Def::DCMagical {
            loc: Loc { line: 1, col: 1 },
            content: content.clone(),
        },
        IrDef::CIntro { content } => Def::DCIntro {
            loc: Loc { line: 1, col: 1 },
            content: content.clone(),
        },
    }
}

fn find_and_emit_func_ir(defs: &[IrDef], name: &str) -> String {
    for d in defs {
        if let IrDef::Func { name: n, type_params, params, returns, body_stmts, body_result, .. } = d {
            if n == name {
                return emit_normal_func(name, type_params, params, returns, body_stmts, body_result, String::new(), 1);
            }
        }
    }
    String::new()
}

fn collect_exported_rec_ir(defs: &[IrDef], sym: &SymbolTable, current_modules: &[String], result: &mut String) {
    for def in defs.iter() {
        match def {
            IrDef::Module { name, defs: inner_defs } => {
                let mut new_modules = current_modules.to_vec();
                new_modules.push(name.clone());
                let parts = module_parts(name);
                let ns_start: String = parts
                    .iter()
                    .map(|p| format!("namespace {} {{\n\n", p))
                    .collect();
                let ns_end: String = parts.iter().map(|_| "}\n\n".to_string()).collect();
                result.push_str(&ns_start);
                collect_exported_rec_ir(inner_defs, sym, &new_modules, result);
                result.push_str(&ns_end);
                return;
            }
            IrDef::Export(symbol) => {
                let decl = if let Some(s) = sym.lookup_struct(symbol) {
                    let field_strs: String = s
                        .fields
                        .iter()
                        .map(|f| format!("  {} {};\n", cxx_type(&f.typ), f.name))
                        .collect();
                    format!("struct {} {{\n{}}};\n\n", s.name, field_strs)
                } else if let Some(e) = sym.lookup_enum(symbol) {
                    emit_enum_def(&e.name, &e.type_params, &e.variants, String::new(), 0)
                } else if let Some(f) = sym.lookup_function(symbol) {
                    if f.type_params.is_empty() {
                        cxx_func_decl(&f.name, &f.params, &f.return_typ)
                    } else {
                        find_and_emit_func_ir(defs, symbol)
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

fn generate_test_ir(defs: &[IrDef]) -> String {
    let modname = defs
        .iter()
        .find_map(|d| match d {
            IrDef::Module { name, .. } => Some(name.clone()),
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
        if let IrDef::Test { name, body_stmts, body_result } = def {
            test_names.push(name.clone());
            let signature = format!("mvp_builtin_int {}", name);
            let stmt_strs: String = body_stmts.iter().map(|s| emit_stmt(s, 1)).collect();
            let ret_line = match body_result {
                Some(expr) => format!("  return {};\n", emit_expr(expr, 1, Some("mvp_builtin_int"))),
                None => "  return mvp_builtin_void;\n".into(),
            };
            body.push_str(&format!(
                "{} {{\n{}{}{}}}\n\n",
                signature,
                stmt_strs,
                ret_line,
                ""
            ));
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

// ===== ENTRY POINT =====

pub fn build_ir(defs: &[Def]) -> [String; 3] {
    record_enum_defs_ir(defs);
    reset_closure_registry_ir();

    let mut ctx = IrContext::new();
    let ir_defs: Vec<IrDef> = lower_defs(&mut ctx, defs);
    let ir_defs = optimize_defs(ir_defs);

    let header_content = generate_header_ir(&ir_defs);
    let scope_parts = generate_with_scope_ir(&ir_defs, None);
    let closure_defs = take_closure_defs_ir();
    let preamble = "\
#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <mvp_builtin.h>

template<class R, class... A>
struct mvp_closure {
    void* env;
    R (*fn)(void*, A...);
    void (*dtor)(void*);

    ~mvp_closure() { if (env && dtor) dtor(env); }
    mvp_closure() : env(nullptr), fn(nullptr), dtor(nullptr) {}
    mvp_closure(void* e, R(*f)(void*, A...), void(*d)(void*)) : env(e), fn(f), dtor(d) {}
    mvp_closure(mvp_closure&& o) : env(o.env), fn(o.fn), dtor(o.dtor) { o.env = nullptr; }
    mvp_closure(const mvp_closure&) = delete;
    mvp_closure& operator=(mvp_closure&& o) { if (this != &o) { this->~mvp_closure(); env = o.env; fn = o.fn; dtor = o.dtor; o.env = nullptr; } return *this; }
    mvp_closure& operator=(const mvp_closure&) = delete;
    R operator()(A... a) const { return fn(env, a...); }
};


using namespace std;

";
    let program = format!(
        "{}{}{}\n{}\n{}\n",
        preamble, scope_parts.includes, closure_defs, scope_parts.defs_str, scope_parts.main_functions
    );
    let test = generate_test_ir(&ir_defs);
    [program, header_content, test]
}
