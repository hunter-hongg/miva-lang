open Ast

(* 符号表类型定义 *)
type symbol_table = {
  module_name: string;                              (* 模块名 *)
  functions: (string * param list * typ option * function_safety) list; (* 函数名 -> 参数列表和返回类型和安全属性 *)
  structs: (string * (string * typ) list) list;      (* 结构体名 -> 字段列表 *)
  exported_functions: (string * param list * typ option * function_safety) list;                  (* 导出的函数名列表 *)
  files: string list
}

(* 函数安全属性 *)
and function_safety =
  | Safe
  | Unsafe
  | Trusted

(* 空符号表 *)
val empty_symbol_table : symbol_table

(* 构建整个程序的符号表 *)
val build_symbol_table : Ast.def list -> symbol_table

(* 构建完整的依赖图 *)
val build_dependency_graph : string -> symbol_table -> Dependency_graph.t

(* 获取函数的安全属性 *)
val get_function_safety : string -> symbol_table -> function_safety option

(* 检查循环依赖 *)
val check_circular_dependencies : string list -> unit

(* 检查程序中的符号重复定义 *)
val check_symbols : Ast.def list -> unit
