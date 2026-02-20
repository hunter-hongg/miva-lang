type typ =
  | TInt
  | TBool
  | TFloat32
  | TFloat64
  | TChar
  | TString
  | TArray of typ
  | TStruct of string * (string * typ) list

type expr =
  | EInt of int64
  | EBool of bool
  | EFloat of float
  | EChar of char
  | EString of string
  | EVar of string
  | EMove of string
  | EClone of string
  | EStructLit of string * (string * expr) list
  | EFieldAccess of expr * string
  | EBinOp of binop * expr * expr
  | EIf of expr * expr * expr option
  | EChoose of expr * (expr * expr) list * expr option  (* choose (var) { when (val) { ... } otherwise { ... } } *)
  | ECall of string * expr list
  | ECast of expr * typ
  | EBlock of stmt list * expr option  (* 块级表达式：语句列表 + 可选的返回表达式 *)
  | EArrayLit of expr list  (* 数组字面量：[]{expr1, expr2, ...} *)

and binop =
  | Add | Sub | Mul | Eq | Neq

and param =
  | PRef of string * typ
  | POwn of string * typ

and stmt =
  | SLet of bool * string * expr  (* true = mutable *)
  | SAssign of string * expr      (* 赋值语句：变量名 = 表达式 *)
  | SReturn of expr
  | SExpr of expr

and def =
  | DStruct of string * (string * typ) list
  | DFunc of string * param list * typ option * expr
  | DFuncUnsafe of string * param list * typ option * expr
  | DFuncTrusted of string * param list * typ option * expr
  | DCFuncUnsafe of string * param list * typ option * string  (* C unsafe function with embedded C code *)
  | DModule of string
  | SExport of string
  | SImport of string