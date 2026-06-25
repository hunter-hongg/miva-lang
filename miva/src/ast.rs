use serde::{Deserialize, Serialize};

#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Loc {
    pub line: i64,
    pub col: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AstFile {
    pub defs: Vec<Def>,
    pub files: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Safety {
    Safe,
    Unsafe,
    Trusted,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDef {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: Typ,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueField {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenCase {
    pub when: Box<Expr>,
    pub then: Box<Expr>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum Typ {
    #[serde(rename = "int")]
    TInt,
    #[serde(rename = "bool")]
    TBool,
    #[serde(rename = "float32")]
    TFloat32,
    #[serde(rename = "float64")]
    TFloat64,
    #[serde(rename = "char")]
    TChar,
    #[serde(rename = "string")]
    TString,
    #[serde(rename = "array")]
    TArray { of: Box<Typ> },
    #[serde(rename = "struct")]
    TStruct { name: String, fields: Vec<FieldDef> },
    #[serde(rename = "ptr")]
    TPtr { to: Box<Typ> },
    #[serde(rename = "box")]
    TBox { of: Box<Typ> },
    #[serde(rename = "null")]
    TNull,
    #[serde(rename = "ptrany")]
    TPtrAny,
    #[serde(rename = "invalid")]
    TInvalid,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BinOp {
    #[serde(rename = "add")]
    Add,
    #[serde(rename = "sub")]
    Sub,
    #[serde(rename = "mul")]
    Mul,
    #[serde(rename = "eq")]
    Eq,
    #[serde(rename = "neq")]
    Neq,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum Param {
    #[serde(rename = "ref")]
    PRef { name: String, #[serde(rename = "type")] typ: Typ },
    #[serde(rename = "own")]
    POwn { name: String, #[serde(rename = "type")] typ: Typ },
}

#[allow(dead_code)]
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum Stmt {
    #[serde(rename = "let")]
    SLet { loc: Loc, mutable: bool, name: String, expr: Box<Expr> },
    #[serde(rename = "letTyped")]
    SLetTyped { loc: Loc, name: String, #[serde(rename = "type")] typ: Typ, expr: Box<Expr> },
    #[serde(rename = "assign")]
    SAssign { loc: Loc, name: String, expr: Box<Expr> },
    #[serde(rename = "return")]
    SReturn { loc: Loc, expr: Box<Expr> },
    #[serde(rename = "expr")]
    SExpr { loc: Loc, expr: Box<Expr> },
    #[serde(rename = "cIntro")]
    SCIntro { loc: Loc, content: String },
    #[serde(rename = "empty")]
    SEmpty { loc: Loc },
}

#[allow(dead_code)]
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum Expr {
    #[serde(rename = "int")]
    EInt { loc: Loc, value: i64 },
    #[serde(rename = "bool")]
    EBool { loc: Loc, value: bool },
    #[serde(rename = "float")]
    EFloat { loc: Loc, value: f64 },
    #[serde(rename = "char")]
    EChar { loc: Loc, value: String },
    #[serde(rename = "string")]
    EString { loc: Loc, value: String },
    #[serde(rename = "var")]
    EVar { loc: Loc, name: String },
    #[serde(rename = "move")]
    EMove { loc: Loc, name: String },
    #[serde(rename = "clone")]
    EClone { loc: Loc, name: String },
    #[serde(rename = "structLit")]
    EStructLit { loc: Loc, name: String, fields: Vec<ValueField> },
    #[serde(rename = "fieldAccess")]
    EFieldAccess { loc: Loc, expr: Box<Expr>, field: String },
    #[serde(rename = "binOp")]
    EBinOp { loc: Loc, op: BinOp, left: Box<Expr>, right: Box<Expr> },
    #[serde(rename = "if")]
    EIf { loc: Loc, cond: Box<Expr>, then: Box<Expr>, #[serde(rename = "else", default)] else_: Option<Box<Expr>> },
    #[serde(rename = "choose")]
    EChoose { loc: Loc, var: Box<Expr>, cases: Vec<WhenCase>, #[serde(default)] otherwise: Option<Box<Expr>> },
    #[serde(rename = "call")]
    ECall { loc: Loc, name: String, args: Vec<Expr> },
    #[serde(rename = "macro")]
    EMacro { loc: Loc, name: String, args: Vec<Expr> },
    #[serde(rename = "cast")]
    ECast { loc: Loc, expr: Box<Expr>, #[serde(rename = "to")] to: Typ },
    #[serde(rename = "block")]
    EBlock { loc: Loc, stmts: Vec<Stmt>, #[serde(default)] result: Option<Box<Expr>> },
    #[serde(rename = "arrayLit")]
    EArrayLit { loc: Loc, values: Vec<Expr> },
    #[serde(rename = "void")]
    EVoid { loc: Loc },
    #[serde(rename = "addr")]
    EAddr { loc: Loc, expr: Box<Expr> },
    #[serde(rename = "deref")]
    EDeref { loc: Loc, expr: Box<Expr> },
    #[serde(rename = "while")]
    EWhile { loc: Loc, cond: Box<Expr>, body: Box<Expr> },
    #[serde(rename = "loop")]
    ELoop { loc: Loc, body: Box<Expr> },
    #[serde(rename = "for")]
    EFor { loc: Loc, var: String, range: Box<Expr>, body: Box<Expr> },
}

#[allow(dead_code)]
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ImplOp {
    #[serde(rename = "op_add")]
    ImAdd,
    #[serde(rename = "op_sub")]
    ImSub,
    #[serde(rename = "op_mul")]
    ImMul,
    #[serde(rename = "op_eq")]
    ImEq,
    #[serde(rename = "op_neq")]
    ImNeq,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplExpr {
    pub op: ImplOp,
    pub func: String,
    pub loc: Loc,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum Def {
    #[serde(rename = "struct")]
    DStruct { loc: Loc, name: String, fields: Vec<FieldDef> },
    #[serde(rename = "func")]
    DFunc { loc: Loc, name: String, params: Vec<Param>, #[serde(default)] returns: Option<Typ>, body: Box<Expr>, safety: Safety },
    #[serde(rename = "cFunc")]
    DCFuncUnsafe { loc: Loc, name: String, params: Vec<Param>, #[serde(default)] returns: Option<Typ>, code: String, safety: Safety },
    #[serde(rename = "test")]
    DTest { loc: Loc, name: String, body: Box<Expr> },
    #[serde(rename = "module")]
    DModule { loc: Loc, name: String },
    #[serde(rename = "export")]
    SExport { loc: Loc, symbol: String },
    #[serde(rename = "import")]
    SImport { loc: Loc, path: String },
    #[serde(rename = "importAs")]
    SImportAs { loc: Loc, path: String, alias: String },
    #[serde(rename = "importHere")]
    SImportHere { loc: Loc, path: String },
    #[serde(rename = "cMagical")]
    DCMagical { loc: Loc, content: String },
    #[serde(rename = "cIntro")]
    DCIntro { loc: Loc, content: String },
    #[serde(rename = "impl")]
    DImpl { loc: Loc, #[serde(rename = "struct")] struct_name: String, impls: Vec<ImplExpr> },
}
