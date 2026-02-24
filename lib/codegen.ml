open Ast
open Symbol_table

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

let write_file filename content =
  let channel = open_out filename in  (* 打开文件，如果存在则覆盖 *)
  output_string channel content;
  close_out channel 

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let get_head list = 
  match list with
  | h :: _ -> h
  | [] -> ""
let rec cxx_type_of_typ = function
  | TInt -> "mvp_builtin_int"
  | TBool -> "mvp_builtin_boolean"
  | TFloat64 -> "mvp_builtin_float"
  | TChar -> "mvp_builtin_byte"
  | TString -> "mvp_builtin_string"
  | TArray typ -> "std::vector<" ^ cxx_type_of_typ typ ^ ">"
  | TStruct (name, _) -> name
  | _ -> failwith "The type is not supported in C++ code generation"

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
  | EInt i -> "static_cast<mvp_builtin_int>(" ^ Int64.to_string i ^ ")"
  | EBool b -> if b then "true" else "false"
  | EFloat f -> string_of_float f
  | EChar c -> "'" ^ Char.escaped c ^ "'"
  | EString s -> "mvp_builtin_string(\"" ^ s ^ "\")"
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
      let call_name = match name with
        | "print" -> "mvp_print"
        | "prints" -> "mvp_prints"
        | "println" -> "mvp_println"
        | "printlns" -> "mvp_printlns"
        | "error" -> "mvp_error"
        | "errors" -> "mvp_errors"
        | "errorln" -> "mvp_errorln"
        | "errorlns" -> "mvp_errorlns"
        | "exit" -> "mvp_exit"
        | "abort" -> "mvp_abort"
        | "panic" -> "mvp_panic"
        | "string_concat" -> "mvp_string_concat"
        | "string_parse" -> "mvp_string_parse"
        | "string_length" -> "mvp_string_length"
        | _ -> 
            (* 将a.b.c.foo转换为a::b::c::foo *)
            let lst = (String.split_on_char '.' name) in
            match lst with 
            | "ffi" :: rest -> (String.concat "::" rest)
            | _ -> String.concat "::" lst
      in
      call_name ^ "(" ^ String.concat ", " args_str ^ ")"
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
  | EChoose (var_expr, cases, otherwise_opt) ->
      let var_str = cxx_expr_of_expr indent_level ctx var_expr in
      let ind = indent indent_level in
      (* let ind_inner = indent (indent_level + 1) in *)
      
      (* 生成每个when分支 *)
      let cases_str = List.fold_left (fun acc (value_expr, body_expr) ->
          let value_str = cxx_expr_of_expr indent_level ctx value_expr in
          let body_str = cxx_expr_of_expr (indent_level + 1) ctx body_expr in
          acc ^ ind ^ "if (" ^ var_str ^ " == " ^ value_str ^ ") { " ^ body_str ^ " }\n"
        ) "" cases in
      
      (* 生成otherwise分支 *)
      let otherwise_str = match otherwise_opt with
        | Some otherwise_expr ->
            let body_str = cxx_expr_of_expr (indent_level + 1) ctx otherwise_expr in
            ind ^ "else { " ^ body_str ^ " }"
        | None -> "" in
      
      "([&]() {\n" ^ cases_str ^ otherwise_str ^ "\n" ^ ind ^ "\n}())"
  | EArrayLit elems ->
      let elem_strs = List.map (cxx_expr_of_expr indent_level ctx) elems in
      "std::vector{" ^ String.concat ", " elem_strs ^ "}"
  | EVoid -> "mvp_builtin_void"

let cxx_deal_module name = 
    if String.compare name "main" == 0 then 
        "mvp_main"
    else if String.starts_with ~prefix:"std" name then 
        "mvp_std" ^ (if String.length name > 3 && String.get name 3 == '.' then "::" ^ String.concat "::" (List.tl (String.split_on_char '.' name)) else "")
    else
        String.concat "::" (String.split_on_char '.' name)

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
  | DFunc ("main", params, _, body) -> 
      let local_ctx = List.fold_left (fun ctx param -> 
          match param with
          | PRef (pname, ptyp) | POwn (pname, ptyp) -> 
              { ctx with vars = Env.add pname { typ = ptyp; state = `Valid; is_mutable = false } ctx.vars }
        ) ctx params in
      let func_signature = "mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)" in
      let body_str = match body with
        | EBlock (stmts, _) ->
            let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts in
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ 
            ind_inner ^ "return mvp_builtin_void;\n" ^
            ind ^ "}\n\n"
        | _ -> 
            (* 单表达式函数：直接返回表达式 *)
            ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level local_ctx body ^ "; }\n\n"
      in
      body_str
  | DCFuncUnsafe (name, params, ret_typ_opt, body) -> 
      let param_strs = List.map (fun param -> 
          match param with
          | PRef (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " const& " ^ pname
          | POwn (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " " ^ pname
        ) params in
      let ret_type = match ret_typ_opt with
        | Some typ -> cxx_type_of_typ typ
        | None -> "mvp_builtin_unit" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      
      ind ^ func_signature ^ " {\n" ^ 
      body ^ "\n" ^ 
      ind ^ "}\n\n"
  | DFuncUnsafe (name, params, ret_typ_opt, body) -> 
      (* unsafe函数生成与普通函数相同，只是语义检查阶段跳过 *)
      let param_strs = List.map (fun param -> 
          match param with
          | PRef (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " const& " ^ pname
          | POwn (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " " ^ pname
        ) params in
      let ret_type = match ret_typ_opt with
        | Some typ -> cxx_type_of_typ typ
        | None -> "mvp_builtin_unit" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      
      (* Create a local context with function parameters *)
      let local_ctx = List.fold_left (fun ctx param -> 
          match param with
          | PRef (pname, ptyp) | POwn (pname, ptyp) -> 
              { ctx with vars = Env.add pname { typ = ptyp; state = `Valid; is_mutable = false } ctx.vars }
        ) ctx params in

      let body_str = match body with
        | EBlock (stmts, expr_opt) ->
            let stmt_strs, expr_str = 
              match ret_type with
              | "mvp_builtin_unit" ->
                  let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None ->
                        ind_inner ^ "return mvp_builtin_void;\n"
                  in
                  (stmt_strs, expr_str)
              | _ ->
                  let stmt_strs, last_expr = 
                    match List.rev stmts with
                    | SExpr expr :: rest ->
                        let rev_stmts = List.rev rest in
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) rev_stmts, Some expr)
                    | _ ->
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts, None)
                  in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None ->
                        match last_expr with
                        | Some expr ->
                            ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                        | None -> ""
                  in
                  (stmt_strs, expr_str)
            in
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ expr_str ^ 
            ind ^ "}\n\n"
        | _ -> 
            match ret_type with
            | "mvp_builtin_unit" ->
                ind ^ func_signature ^ " { return mvp_builtin_void; }\n\n"
            | _ ->
                ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level local_ctx body ^ "; }\n\n"
      in
      body_str
  | DFuncTrusted (name, params, ret_typ_opt, body) -> 
      (* trusted函数生成与unsafe函数相同，语义检查阶段跳过 *)
      let param_strs = List.map (fun param -> 
          match param with
          | PRef (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " const& " ^ pname
          | POwn (pname, ptyp) -> 
              cxx_type_of_typ ptyp ^ " " ^ pname
        ) params in
      let ret_type = match ret_typ_opt with
        | Some typ -> cxx_type_of_typ typ
        | None -> "mvp_builtin_unit" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      
      (* Create a local context with function parameters *)
      let local_ctx = List.fold_left (fun ctx param -> 
          match param with
          | PRef (pname, ptyp) | POwn (pname, ptyp) -> 
              { ctx with vars = Env.add pname { typ = ptyp; state = `Valid; is_mutable = false } ctx.vars }
        ) ctx params in

      let body_str = match body with
        | EBlock (stmts, expr_opt) ->
            let stmt_strs, expr_str = 
              match ret_type with
              | "mvp_builtin_unit" ->
                  let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None ->
                        ind_inner ^ "return mvp_builtin_void;\n"
                  in
                  (stmt_strs, expr_str)
              | _ ->
                  let stmt_strs, last_expr = 
                    match List.rev stmts with
                    | SExpr expr :: rest ->
                        let rev_stmts = List.rev rest in
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) rev_stmts, Some expr)
                    | _ ->
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts, None)
                  in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None ->
                        match last_expr with
                        | Some expr ->
                            ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                        | None -> ""
                  in
                  (stmt_strs, expr_str)
            in
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ expr_str ^ 
            ind ^ "}\n\n"
        | _ -> 
            match ret_type with
            | "mvp_builtin_unit" ->
                ind ^ func_signature ^ " { return mvp_builtin_void; }\n\n"
            | _ ->
                ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level local_ctx body ^ "; }\n\n"
      in
      body_str 
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
        | None -> "mvp_builtin_unit" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      
      (* Create a local context with function parameters *)
      let local_ctx = List.fold_left (fun ctx param -> 
          match param with
          | PRef (pname, ptyp) | POwn (pname, ptyp) -> 
              { ctx with vars = Env.add pname { typ = ptyp; state = `Valid; is_mutable = false } ctx.vars }
        ) ctx params in

      (* Helper function to extract the last expression from statements for auto-return *)
      (* let extract_last_expr stmts =
        match List.rev stmts with
        | SExpr expr :: _ -> Some expr
        | SReturn expr :: _ -> Some expr
        | _ -> None
      in *)
      
      let body_str = match body with
        | EBlock (stmts, expr_opt) ->
            (* 根据Miva规范：函数自动返回最后一个表达式 *)
            let stmt_strs, expr_str = 
              match ret_type with
              | "mvp_builtin_unit" ->
                  (* unit类型：所有语句都执行，最后插入return mvp_builtin_void; *)
                  let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None ->
                        ind_inner ^ "return mvp_builtin_void;\n"
                  in
                  (stmt_strs, expr_str)
              | _ ->
                  (* 非unit类型：最后一个表达式语句只作为返回值，不作为普通语句 *)
                  let stmt_strs, last_expr = 
                    match List.rev stmts with
                    | SExpr expr :: rest ->
                        (* 最后一个语句是表达式，不将其作为普通语句生成，只作为返回值 *)
                        let rev_stmts = List.rev rest in
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) rev_stmts, Some expr)
                    | _ ->
                        (* 最后一个语句不是表达式，或者没有语句 *)
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts, None)
                  in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None ->
                        match last_expr with
                        | Some expr ->
                            ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                        | None -> ""
                  in
                  (stmt_strs, expr_str)
            in
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ expr_str ^ 
            ind ^ "}\n\n"
        | _ -> 
            (* 单表达式函数：直接返回表达式 *)
            match ret_type with
            | "mvp_builtin_unit" ->
                (* 返回类型是unit，直接返回mvp_builtin_void *)
                ind ^ func_signature ^ " { return mvp_builtin_void; }\n\n"
            | _ ->
                ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level local_ctx body ^ "; }\n\n"
      in
      body_str
  | DModule s -> (
      "namespace " ^ cxx_deal_module s ^ " {\n\n"
  )
  | SExport _ ->
      ""
  | SImport import -> (
    if String.starts_with ~prefix:"c:" import then 
        let import_str = String.sub import 2 (String.length import - 2) in
        "#include <" ^ import_str ^ ">\n"
    else (
        let toml = read_file "miva.toml" in
        match Toml.Parser.from_string toml with 
        | `Ok table -> (
        try 
            match Toml.Types.Table.find (Toml.Min.key "project") table with 
            | Toml.Types.TTable t -> (
            match Toml.Types.Table.find (Toml.Min.key "name") t with 
            | Toml.Types.TString s -> (
                if String.starts_with ~prefix:s import then
                let res = "#include <src/" ^
                    (String.concat "/" ((String.split_on_char '/' import) |> List.tl))
                    ^ ".h>\n" in
                res
                else
                let pstk = String.split_on_char '/' import in
                let res = "#include <" ^ 
                    get_head pstk ^ "/src/" ^ 
                    (String.concat "/" (List.tl pstk)) ^ ".h>\n" in
                res
            )
            | _ -> Printf.eprintf "Warning: import failed\n%!"; ""
            )
            | _ -> Printf.eprintf "Warning: import failed\n%!"; ""
        with 
            | _ -> Printf.eprintf "Warning: import failed\n%!"; ""
        )
        | _ -> Printf.eprintf "Warning: import failed\n%!"; "" 
    ))
  | DTest(_, _) -> ""


(* 生成函数声明（用于头文件） *)
let cxx_func_declaration func_name params ret_typ_opt =
  let param_strs = List.map (fun param -> 
      match param with
      | PRef (pname, ptyp) -> 
          cxx_type_of_typ ptyp ^ " const& " ^ pname
      | POwn (pname, ptyp) -> 
          cxx_type_of_typ ptyp ^ " " ^ pname
    ) params in
  let ret_type = match ret_typ_opt with
    | Some typ -> cxx_type_of_typ typ
    | None -> "mvp_builtin_unit" in
  ret_type ^ " " ^ func_name ^ "(" ^ String.concat ", " param_strs ^ ");\n"

let generate_test indent_level defs = 
    let ind = indent indent_level in
    let ind_inner = indent (indent_level + 1) in
    let local_ctx = { vars = Env.empty; types = Env.empty} in
    let header = "#include <mvp_builtin.h>\n" in
    let body = ref "" in 
    let found = ref false in
    List.iter (fun def ->
      match def with
      | DTest (name, expr) ->
      let param_strs = ["";] in
      let ret_type = "mvp_builtin_int" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      let body_str = match expr with
        | EBlock (stmts, expr_opt) ->
            let stmt_strs, expr_str = 
                let stmt_strs, last_expr = 
                match List.rev stmts with
                | SExpr expr :: rest ->
                    let rev_stmts = List.rev rest in
                    (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) rev_stmts, Some expr)
                | _ ->
                    (List.map (cxx_stmt_of_stmt (indent_level + 1) local_ctx) stmts, None)
                in
                let expr_str = match expr_opt with
                | Some expr -> 
                    ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                | None ->
                    match last_expr with
                    | Some expr ->
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) local_ctx expr ^ ";\n"
                    | None -> ""
                in
                (stmt_strs, expr_str)
            in
            found := true;
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ expr_str ^ 
            ind ^ "}\n\n"
        | _ -> 
            ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level local_ctx expr ^ "; }\n\n"
      in
      body := ((!body) ^ body_str)
      | _ -> ()
    ) defs;
    if not (!found) then "" else
    header ^ !body ^ "int main() {\n" ^
      "mvp_builtin_int passed = 0, failed = 0;\n" ^
      String.concat "" (List.map( fun def -> (
        match def with
        | DTest (name, _) -> (
            "try {\n" ^
            "auto res = " ^
            name ^ "();\n" ^
            "if (res != 0) {\n" ^ 
            "throw std::runtime_error(\"Test failed: " ^ name ^ " returns none-zero.\");\n" ^
            "}\n" ^ 
            "passed++;\n" ^
            "} catch (const std::exception& e) {\n" ^
            " mvp_errorlns(\"Error testing " ^ name ^ " : panic: \", e.what()); failed++;\n" ^
            "}\n"  
        )
        | _ -> ""
      )) defs) ^
      "if (failed == 0) {mvp_println(\"All tests passed.\");return 0;}\n" ^
      "else {mvp_printlns(\"Test failed. Failed: \", failed, \" Passed: \", passed);return 1;}" ^
    "}"

(* 生成头文件内容 *)
let generate_header defs =
  let symbol_table = build_symbol_table defs in
  
  let header_guard = "#pragma once\n\n" in
  
  let includes = "#include <mvp_builtin.h>\n\n" in
  
  (* 收集所有导出的函数声明和结构体声明，考虑模块结构 *)
  let rec collect_exported_decls defs current_modules = 
    match defs with
    | [] -> ""
    | def :: rest ->
        match def with
        | DModule module_name ->
            (* 进入新模块，生成嵌套的namespace *)
            let new_modules = current_modules @ [module_name] in
            let module_parts = String.split_on_char '.' module_name in
            let module_parts = if get_head module_parts = "std" then
              "mvp_std" :: (module_parts |> List.tl)
            else if get_head module_parts = "main" then
              let res = "mvp_main" :: (module_parts |> List.tl) in
              res
            else
              module_parts in
            let module_start = List.fold_left (fun acc part -> acc ^ "namespace " ^ part ^ " {\n\n") "" module_parts in
            let inner_content = collect_exported_decls rest new_modules in
            let module_end = List.fold_left (fun acc _ -> acc ^ "}\n\n") "" module_parts in
            module_start ^ inner_content ^ module_end
        | SExport name ->
            (* 先检查是否是结构体 *)
            let struct_info = List.find_opt (fun (sname, _) -> sname = name) symbol_table.structs in
            let decl = match struct_info with
              | Some (sname, fields) ->
                  (* 生成结构体完整定义，包括所有字段 *)
                  "struct " ^ sname ^ " {\n" ^
                  List.fold_left (fun acc (fname, ftyp) ->
                      acc ^ "  " ^ cxx_type_of_typ ftyp ^ " " ^ fname ^ ";\n"
                    ) "" fields ^
                  "};\n\n"
              | None ->
                  (* 不是结构体，检查是否是函数 *)
                  let func_info = List.find_opt (fun (fname, _, _, _) -> fname = name) symbol_table.functions in
                  match func_info with
                  | Some (fname, params, ret_typ_opt, _) ->
                      cxx_func_declaration fname params ret_typ_opt
                  | None ->
                      (* 如果符号表中找不到，发出警告但继续处理 *)
                      Printf.eprintf "Warning: '%s' not found in symbol table, skipping export\n" name;
                      ""
            in
            decl ^ collect_exported_decls rest current_modules
        | _ ->
            collect_exported_decls rest current_modules
  in
  let exported_decls = collect_exported_decls defs [] in
  
  let footer = "\n" in
  
  if exported_decls = "" then
    "" (* 没有导出的函数，不生成头文件 *)
  else
    header_guard ^ includes ^ exported_decls ^ footer

let build_ir _fpath defs = 
  (* 生成头文件 *)
  let header_content = generate_header defs in
  
  let header = "#include <iostream>\n" ^
  "#include <string>\n" ^
  "#include <vector>\n" ^
  "#include <cstdint>\n" ^
  "#include <mvp_builtin.h>\n" ^
  "\n" ^
  "\nusing namespace std;\n\n" in
  
  (* Create a minimal context with empty types and vars *)
  let ctx = { types = Env.empty; vars = Env.empty } in
  
  (* Helper function to process definitions with module scope handling *)
  let rec generate_with_scope ctx current_module main_functions includes defs_str defs = 
    match defs with
    | [] -> (includes, defs_str, main_functions)
    | def :: rest -> 
        match def with
        | DModule module_name ->
            (* Start nested module scopes *)
            let module_parts = String.split_on_char '.' module_name in
            let module_parts = if get_head module_parts = "std" then
              "mvp_std" :: (module_parts |> List.tl)
            else if get_head module_parts = "main" then
              let res = "mvp_main" :: (module_parts |> List.tl) in
              res
            else
              module_parts in
            let module_start = List.fold_left (fun acc part -> acc ^ "namespace " ^ part ^ " {\n\n") "" module_parts in
            let inner_includes, inner_content, new_main_functions = generate_with_scope ctx (Some module_name) main_functions "" "" rest in
            let module_end = List.fold_left (fun acc _ -> acc ^ "}\n\n") "" module_parts in
            (includes ^ inner_includes, defs_str ^ module_start ^ inner_content ^ module_end, new_main_functions)
        | DFunc ("main", params, _, body) ->
            (* 处理main函数：生成mvp_main在模块内，main函数在全局 *)
            let mvp_main_def = DFunc ("main", params, None, body) in
            let mvp_main_str = cxx_def_of_def 0 ctx mvp_main_def in
            
            (* 生成全局的main函数，调用模块中的mvp_main *)
            let global_main = match current_module with
              | Some module_name ->
                  "int main(int argc, char** argv)\n" ^
                  "{\n" ^
                  "  try {\n" ^
                  "  " ^ cxx_deal_module module_name ^ "::mvp_own_main(argc);\n" ^
                  "  } catch (std::exception& e) {\n" ^
                  "     mvp_errorlns(\"panic: \", e.what());" ^
                  "  }\n" ^
                  "  return 0;\n" ^
                  "}\n\n"
              | None ->
                  "int main(int argc, char** argv)\n" ^
                  "{\n" ^
                  "  mvp_own_main(argc);\n" ^
                  "  return 0;\n" ^
                  "}\n\n"
            in
            
            (* 将mvp_main放在当前模块内，main函数放在全局 *)
            generate_with_scope ctx current_module (main_functions ^ global_main) includes (defs_str ^ mvp_main_str) rest
        | SImport _ ->
            (* 处理导入语句：将其添加到includes中，而不是当前模块的命名空间内 *)
            let import_str = cxx_def_of_def 0 ctx def in
            generate_with_scope ctx current_module main_functions (includes ^ import_str) defs_str rest
        | _ ->
            (* Generate definition in current module scope *)
            let def_str = cxx_def_of_def 0 ctx def in
            generate_with_scope ctx current_module main_functions includes (defs_str ^ def_str) rest
  in
  
  let includes, module_content, main_functions = generate_with_scope ctx None "" "" "" defs in
  let program = header ^ includes ^ module_content ^ main_functions ^ "\n" in
  let test = generate_test 0 defs in
  [program;header_content;test]
