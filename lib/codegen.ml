open Ast

module Env = Map.Make(String)

type var_info = {
  typ: typ;
  mutable state: [`Valid | `Moved];
  is_mutable: bool;
}

type context = {
  types: (string * typ) list Env.t;  (* struct name -> field list *)
  mutable vars: var_info Env.t;
}
let indent n = String.make (n * 2) ' '

let rec cxx_type_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFloat32 -> "float"
  | TFloat64 -> "double"
  | TChar -> "char"
  | TString -> "std::string"
  | TArray typ -> "std::vector<" ^ cxx_type_of_typ typ ^ ">"
  | TStruct (name, _) -> name
  (* | _ -> failwith "Unsupported type in C++ code generation" *)

let rec cxx_stmt_of_stmt indent_level ctx stmt = 
  let ind = indent indent_level in
  (* let ind_inner = indent (indent_level + 1) in *)
  match stmt with
  | SLet (is_mutable, name, expr) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      let mut_str = if is_mutable then "auto " else "const auto " in
      ind ^ mut_str ^ name ^ " = " ^ expr_str ^ ";\n"
  | SReturn expr -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      ind ^ "return " ^ expr_str ^ ";\n"
  | SExpr expr -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      ind ^ expr_str ^ ";\n"
  | SAssign (name, expr) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      ind ^ name ^ " = " ^ expr_str ^ ";\n"

and cxx_expr_of_expr indent_level ctx expr = 
  (* let ind = indent indent_level in *)
  (* let ind_inner = indent (indent_level + 1) in *)
  match expr with
  | EInt i -> Int64.to_string i
  | EBool b -> if b then "true" else "false"
  | EFloat f -> string_of_float f
  | EChar c -> "'" ^ Char.escaped c ^ "'"
  | EString s -> "\"" ^ String.escaped s ^ "\""
  | EVar name -> name
  | EMove name -> "std::move(" ^ name ^ ")"
  | EClone name -> name
  | EStructLit (name, fields) -> 
      if List.length fields = 0 then
        name ^ "{}"
      else
        let temp_var = "__temp_" ^ string_of_int (Random.int 1000) in
        let init_stmts = List.map (fun (fname, fexpr) -> 
            let field_expr = cxx_expr_of_expr (indent_level + 1) ctx fexpr in
            temp_var ^ "." ^ fname ^ " = " ^ field_expr
          ) fields in
        "([&]() { " ^ name ^ " " ^ temp_var ^ "{}; " ^ 
        String.concat "; " init_stmts ^ "; return " ^ temp_var ^ "; }())"
  | EFieldAccess (expr, field) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      expr_str ^ "." ^ field
  | EBinOp (op, e1, e2) -> 
      let op_str = match op with
        | Add -> " + "
        | Sub -> " - "
        | Mul -> " * "
        | Eq -> " == "
        | Neq -> " != " in
      let e1_str = cxx_expr_of_expr indent_level ctx e1 in
      let e2_str = cxx_expr_of_expr indent_level ctx e2 in
      "(" ^ e1_str ^ op_str ^ e2_str ^ ")"
  | EIf (cond, then_expr, else_expr_opt) -> 
      let cond_str = cxx_expr_of_expr indent_level ctx cond in
      let then_str = cxx_expr_of_expr (indent_level + 1) ctx then_expr in
      let else_str = match else_expr_opt with
        | Some else_expr -> 
            " else " ^ cxx_expr_of_expr (indent_level + 1) ctx else_expr
        | None -> ""
      in
      "if (" ^ cond_str ^ ") { " ^ then_str ^ " }" ^ else_str
  | ECall (name, args) -> 
      let args_str = List.map (cxx_expr_of_expr indent_level ctx) args in
      name ^ "(" ^ String.concat ", " args_str ^ ")"
  | ECast (expr, typ) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      "static_cast<" ^ cxx_type_of_typ typ ^ ">(" ^ expr_str ^ ")"
  | EBlock (stmts, expr_opt) -> 
      let ind = indent indent_level in
      let ind_inner = indent (indent_level + 1) in
      
      (* 创建一个新的变量环境，继承自当前环境 *)
      let local_ctx = { ctx with vars = ctx.vars } in
      
      (* 处理块内的所有语句，包括变量声明 *)
      let stmt_strs = List.fold_left (fun acc stmt -> 
          match stmt with
          | SLet (is_mutable, name, expr) ->
              (* 处理变量初始化表达式 *)
              let expr_str = cxx_expr_of_expr (indent_level + 1) local_ctx expr in
              
              (* 生成变量声明的C++代码 *)
              let mut_str = if is_mutable then "auto " else "const auto " in
              acc ^ ind_inner ^ mut_str ^ name ^ " = " ^ expr_str ^ ";\n"
          | _ ->
              acc ^ cxx_stmt_of_stmt (indent_level + 1) local_ctx stmt
        ) "" stmts in
      
      (* 处理块的最后表达式 *)
      let expr_str = match expr_opt with
        | Some expr -> ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
        | None -> "" in
      
      "{\n" ^ stmt_strs ^ expr_str ^ ind ^ "}"


let cxx_def_of_def indent_level ctx def = 
  let ind = indent indent_level in
  let ind_inner = indent (indent_level + 1) in
  match def with
  | DStruct (name, fields) -> 
      let field_strs = List.map (fun (fname, ftyp) -> 
          ind_inner ^ cxx_type_of_typ ftyp ^ " " ^ fname ^ ";\n"
        ) fields in
      ind ^ "struct " ^ name ^ " {\n" ^ 
      String.concat "" field_strs ^ 
      ind ^ "};\n\n"
  | DFunc (name, params, ret_typ_opt, body) -> 
      let param_strs = List.map (fun param -> 
          match param with
          | PRef (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " const& " ^ pname
          | POwn (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " " ^ pname
        ) params in
      let ret_type = match ret_typ_opt with
        | Some typ -> cxx_type_of_typ typ
        | None -> "void" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      
      (* Create a local context with function parameters *)
      let local_ctx = List.fold_left (fun ctx param -> 
          match param with
          | PRef (pname, ptyp) | POwn (pname, ptyp) -> 
              { ctx with vars = Env.add pname { typ = ptyp; state = `Valid; is_mutable = false } ctx.vars }
        ) ctx params in

      (* Helper function to extract the last expression from statements for auto-return *)
      let extract_last_expr stmts =
        match List.rev stmts with
        | SExpr expr :: _ -> Some expr
        | SReturn expr :: _ -> Some expr
        | _ -> None
      in
      
      let body_str = match body with
        | EBlock (stmts, expr_opt) ->
            (* 根据MVP规范：函数自动返回最后一个表达式 *)
            let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts in
            let expr_str = match expr_opt with
              | Some expr -> 
                  (* 如果显式指定了返回表达式，使用它 *)
                  ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
              | None ->
                  (* 如果没有显式指定，自动返回最后一个表达式 *)
                  match extract_last_expr stmts with
                  | Some last_expr ->
                      ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx last_expr ^ ";\n"
                  | None -> ""
            in
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ expr_str ^ 
            ind ^ "}\n\n"
        | _ -> 
            (* 单表达式函数：直接返回表达式 *)
            ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level local_ctx body ^ "; }\n\n"
      in
      body_str

let build_ir defs = 
  let header = "#include <iostream>\n#include <string>\n#include <vector>\n#include <cstdint>\n\nusing namespace std;\n\n" in
  
  (* Create a minimal context with empty types and vars *)
  let ctx = { types = Env.empty; vars = Env.empty } in
  
  let rec generate ctx defs_str defs = 
    match defs with
    | [] -> defs_str
    | def :: rest -> 
        let def_str = cxx_def_of_def 0 ctx def in
        generate ctx (defs_str ^ def_str) rest
  in
  
  let program = header ^ generate ctx "" defs ^ "\n" in
  
  (* Write to a temporary file for testing *)
  let temp_file = "temp.cpp" in
  let oc = open_out temp_file in
  output_string oc program;
  close_out oc;
 (* Create a minimal context with empty types and vars *)
 
  program