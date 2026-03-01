(* 位置信息类型 *)
type loc = {
  line: int;  (* 行号，从1开始 *)
  col: int;   (* 列号，从1开始 *)
}

type typ =
  | TInt
  | TBool
  | TFloat32
  | TFloat64
  | TChar
  | TString
  | TArray of typ
  | TStruct of string * (string * typ) list
  | TPtr of typ

type expr =
  | EInt of loc * int64
  | EBool of loc * bool
  | EFloat of loc * float
  | EChar of loc * char
  | EString of loc * string
  | EVar of loc * string
  | EMove of loc * string
  | EClone of loc * string
  | EStructLit of loc * string * (string * expr) list
  | EFieldAccess of loc * expr * string
  | EBinOp of loc * binop * expr * expr
  | EIf of loc * expr * expr * expr option
  | EChoose of loc * expr * (expr * expr) list * expr option  (* choose (var) { when (val) { ... } otherwise { ... } } *)
  | ECall of loc * string * expr list
  | ECast of loc * expr * typ
  | EBlock of loc * stmt list * expr option  (* 块级表达式：语句列表 + 可选的返回表达式 *)
  | EArrayLit of loc * expr list  (* 数组字面量：[]{expr1, expr2, ...} *)
  | EVoid of loc 
  | EAddr of loc * expr
  | EDeref of loc * expr

and binop =
  | Add | Sub | Mul | Eq | Neq

and param =
  | PRef of string * typ
  | POwn of string * typ

and stmt =
  | SLet of loc * bool * string * expr  (* true = mutable *)
  | SAssign of loc * string * expr      (* 赋值语句：变量名 = 表达式 *)
  | SReturn of loc * expr
  | SExpr of loc * expr

and def =
  | DStruct of loc * string * (string * typ) list
  | DFunc of loc * string * param list * typ option * expr
  | DFuncUnsafe of loc * string * param list * typ option * expr
  | DFuncTrusted of loc * string * param list * typ option * expr
  | DCFuncUnsafe of loc * string * param list * typ option * string  (* C unsafe function with embedded C code *)
  | DTest of loc * string * expr  (* 测试函数：名称和主体，固定无参返回int *)
  | DModule of loc * string
  | SExport of loc * string
  | SImport of loc * string
  | SImportAs of loc * string * string
  | SImportHere of loc * string
