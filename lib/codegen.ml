open Ast
open Symbol_table

module Env = Map.Make(String)

let parse_input_file = Wrapper.parse_input_file

let indent n = String.make (n * 2) ' '

let write_file = Util.write_file

let read_file = Util.read_file

let get_head = Wrapper.get_head

let rec cxx_type_of_typ = function
  | TInt -> "mvp_builtin_int"
  | TBool -> "mvp_builtin_boolean"
  | TFloat64 -> "mvp_builtin_float"
  | TChar -> "mvp_builtin_byte"
  | TString -> "mvp_builtin_string"
  | TArray typ -> "std::vector<" ^ cxx_type_of_typ typ ^ ">"
  | TStruct (name, _) -> name
  | TPtr typ -> cxx_type_of_typ typ ^ "*"
  | TBox typ -> "mvp_builtin_box<" ^ cxx_type_of_typ typ ^ ">"
  | _ -> failwith "The type is not supported in C++ code generation"

let rec cxx_stmt_of_stmt indent_level ctx stmt = 
  let ind = indent indent_level in
  match stmt with
  | SLet (_, is_mutable, name, expr) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      let mut_str = if is_mutable then "auto " else "const auto " in
      ind ^ mut_str ^ name ^ " = " ^ expr_str ^ ";\n" ^
      (
        match expr with 
        | EVar (_, _) -> 
        ind ^ "static_assert(is_copyable_v<decltype(" ^ name 
        ^ ")>, \"A non-Copy type can't be assigned without clone or move\");\n"
        | _ -> ""
      )
  | SReturn (_, expr) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      ind ^ "return " ^ expr_str ^ ";\n"
  | SExpr (_, expr) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      ind ^ expr_str ^ ";\n"
  | SAssign (_, name, expr) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      ind ^ name ^ " = " ^ expr_str ^ ";\n" ^ 
      (
        match expr with 
        | EMove (_, _) -> ""
        | EClone (_, _) -> ""
        | ECall (_, _, _) -> ""
        | EMacro (_, _, _) -> ""
        | _ -> 
        ind ^ "static_assert(is_copyable_v<decltype(" ^ name 
        ^ ")>, \"A non-Copy type can't be assigned without clone or move\");\n"
      )
and cxx_expr_of_expr indent_level ctx expr = 
  match expr with
  | EInt (_, i) -> "static_cast<mvp_builtin_int>(" ^ Int64.to_string i ^ ")"
  | EBool (_, b) -> "bool(" ^ (if b then "true" else "false") ^ ")"
  | EFloat (_, f) -> string_of_float f
  | EChar (_, c) -> "'" ^ Char.escaped c ^ "'"
  | EString (_, s) -> "mvp_builtin_string(\"" ^ s ^ "\")"
  | EVar (_, name) -> name
  | EMove (_, name) -> "std::move(" ^ name ^ ")"
  | EClone (_, name) -> name
  | EStructLit (_, name, fields) -> 
      if List.length fields = 0 then
        name ^ "{}"
      else
        let temp_var = "__temp" in
        let init_stmts = List.map (fun (fname, fexpr) -> 
            let field_expr = cxx_expr_of_expr (indent_level + 1) ctx fexpr in
            temp_var ^ "." ^ fname ^ " = " ^ field_expr
          ) fields in
        "([&]() { " ^ name ^ " " ^ temp_var ^ "{}; " ^ 
        String.concat "; " init_stmts ^ "; return " ^ temp_var ^ "; }())"
  | EFieldAccess (_, expr, field) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      expr_str ^ "." ^ field
  | EBinOp (_, op, e1, e2) -> 
      let op_str = match op with
        | Add -> " + "
        | Sub -> " - "
        | Mul -> " * "
        | Eq -> " == "
        | Neq -> " != " in
      let e1_str = cxx_expr_of_expr indent_level ctx e1 in
      let e2_str = cxx_expr_of_expr indent_level ctx e2 in
      "(" ^ e1_str ^ op_str ^ e2_str ^ ")"
  | EIf (_, cond, then_expr, else_expr_opt) -> 
      let cond_str = cxx_expr_of_expr indent_level ctx cond in
      let then_str = cxx_expr_of_expr (indent_level + 1) ctx then_expr in
      let else_str = match else_expr_opt with
        | Some else_expr -> 
            " else " ^ cxx_expr_of_expr (indent_level + 1) ctx else_expr
        | None -> ""
      in
      "([&]() { if (" ^ cond_str ^ ") { " ^ then_str ^ " ;}" ^ else_str ^ " })()"
  | EWhile (_, cond, body) -> (
      let cond_str = cxx_expr_of_expr indent_level ctx cond in
      let body_str = cxx_expr_of_expr (indent_level + 1) ctx body in
      "([&]() { while (" ^ cond_str ^ ") { " ^ body_str ^ " ;}})()"
  )
  | ELoop (_, body) -> (
      let body_str = cxx_expr_of_expr (indent_level + 1) ctx body in
      "([&]() { for (;;) { " ^ body_str ^ " ;}})()"
  )
  | EFor (_, i, range, body) -> (
      let range_str = cxx_expr_of_expr indent_level ctx range in
      let body_str = cxx_expr_of_expr (indent_level + 1) ctx body in
      "([&]() { for (auto " ^ i ^ " : " ^ range_str ^ ") { " ^ body_str ^ " ;}})()"
  )
  | ECall (_, name, args) -> 
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
        | "string_make" -> "mvp_string_make"
        | "string_from" -> "mvp_to_string"
        | "box_new" -> "mvp_box_new"
        | "box_deref" -> "mvp_box_deref"
        | "range" -> "mvp_range"
        | _ -> 
            (* 将a.b.c.foo转换为a::b::c::foo *)
            let lst = (String.split_on_char '.' name) in
            match lst with 
            | "ffi" :: rest -> (String.concat "::" rest)
            | _ -> String.concat "::" lst
      in
      call_name ^ "(" ^ String.concat ", " args_str ^ ")"
  | ECast (_, expr, typ) -> 
      let expr_str = cxx_expr_of_expr indent_level ctx expr in
      "static_cast<" ^ cxx_type_of_typ typ ^ ">(" ^ expr_str ^ ")"
  | EBlock (_, stmts, expr_opt) -> 
      let ind = indent indent_level in
      let ind_inner = indent (indent_level + 1) in
      
      let stmt_strs = List.fold_left (fun acc stmt -> 
        match stmt with
        | SLet (_, is_mutable, name, expr) ->
            let expr_str = cxx_expr_of_expr (indent_level + 1) ctx expr in
            
            let mut_str = if is_mutable then "auto " else "const auto " in
            acc ^ ind_inner ^ mut_str ^ name ^ " = " ^ expr_str ^ ";\n"
        | _ ->
            acc ^ cxx_stmt_of_stmt (indent_level + 1) ctx stmt
      ) "" stmts in
      
      let expr_str = match expr_opt with
        | Some expr -> ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
        | None -> "" in
      
      "([&]() {\n" ^ stmt_strs ^ expr_str ^ ind ^ "})()"
  | EChoose (_, var_expr, cases, otherwise_opt) ->
      let var_str = cxx_expr_of_expr indent_level ctx var_expr in
      let ind = indent indent_level in
      let cases_str = List.fold_left (fun acc (value_expr, body_expr) ->
          let value_str = cxx_expr_of_expr indent_level ctx value_expr in
          let body_str = cxx_expr_of_expr (indent_level + 1) ctx body_expr in
          acc ^ ind ^ "if (" ^ var_str ^ " == " ^ value_str ^ ") { " ^ body_str ^ " }\n"
        ) "" cases in
      
      let otherwise_str = match otherwise_opt with
        | Some otherwise_expr ->
            let body_str = cxx_expr_of_expr (indent_level + 1) ctx otherwise_expr in
            ind ^ "else { " ^ body_str ^ " }"
        | None -> "" in
      
      "([&]() {\n" ^ cases_str ^ otherwise_str ^ "\n" ^ ind ^ "\n}())"
  | EArrayLit (_, elems) ->
      let elem_strs = List.map (cxx_expr_of_expr indent_level ctx) elems in
      "std::vector{" ^ String.concat ", " elem_strs ^ "}"
  | EVoid _ -> "mvp_builtin_void"
  | EAddr (_, expr) -> "&(" ^ cxx_expr_of_expr indent_level ctx expr ^ ")"
  | EDeref (_, expr) -> "*(" ^ cxx_expr_of_expr indent_level ctx expr ^ ")"
  | EMacro (_, _, _) -> ""

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
  | DStruct (_, name, fields) -> 
      let field_strs = List.map (fun (fname, ftyp) -> 
          ind_inner ^ cxx_type_of_typ ftyp ^ " " ^ fname ^ ";\n"
        ) fields in
      ind ^ "struct " ^ name ^ " {\n" ^ 
      String.concat "" field_strs ^ 
      ind ^ "};\n\n"
  | DFunc (_, "main", _, _, body) -> 
      let func_signature = "mvp_builtin_unit mvp_own_main(mvp_builtin_int argc)" in
      let body_str = match body with
        | EBlock (_, stmts, _) ->
            let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts in
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ 
            ind_inner ^ "return mvp_builtin_void;\n" ^
            ind ^ "}\n\n"
        | _ -> 
            ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level ctx body ^ "; }\n\n"
      in
      body_str
  | DCFuncUnsafe (_, name, params, ret_typ_opt, body) -> 
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
  | DFuncUnsafe (_, name, params, ret_typ_opt, body) -> 
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
      
      let body_str = match body with
        | EBlock (_, stmts, expr_opt) ->
            let stmt_strs, expr_str = 
              match ret_type with
              | "mvp_builtin_unit" ->
                  let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None ->
                        ind_inner ^ "return mvp_builtin_void;\n"
                  in
                  (stmt_strs, expr_str)
              | _ ->
                  let stmt_strs, last_expr = 
                    match List.rev stmts with
                    | SExpr (_, expr) :: rest ->
                        let rev_stmts = List.rev rest in
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) rev_stmts, Some expr)
                    | _ ->
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts, None)
                  in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None ->
                        match last_expr with
                        | Some expr ->
                            ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
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
                ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level ctx body ^ "; }\n\n"
      in
      body_str
  | DFuncTrusted (_, name, params, ret_typ_opt, body) -> 
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
      
      let body_str = match body with
        | EBlock (_, stmts, expr_opt) ->
            let stmt_strs, expr_str = 
              match ret_type with
              | "mvp_builtin_unit" ->
                  let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None ->
                        ind_inner ^ "return mvp_builtin_void;\n"
                  in
                  (stmt_strs, expr_str)
              | _ ->
                  let stmt_strs, last_expr = 
                    match List.rev stmts with
                    | SExpr (_, expr) :: rest ->
                        let rev_stmts = List.rev rest in
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) rev_stmts, Some expr)
                    | _ ->
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts, None)
                  in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None ->
                        match last_expr with
                        | Some expr ->
                            ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
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
                ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level ctx body ^ "; }\n\n"
      in
      body_str 
  | DFunc (_, name, params, ret_typ_opt, body) -> 
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

      let body_str = match body with
        | EBlock (_, stmts, expr_opt) ->
            let stmt_strs, expr_str = 
              match ret_type with
              | "mvp_builtin_unit" ->
                  let stmt_strs = List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None ->
                        ind_inner ^ "return mvp_builtin_void;\n"
                  in
                  (stmt_strs, expr_str)
              | _ ->
                  let stmt_strs, last_expr = 
                    match List.rev stmts with
                    | SExpr (_, expr) :: rest ->
                        let rev_stmts = List.rev rest in
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) rev_stmts, Some expr)
                    | _ ->
                        (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts, None)
                  in
                  let expr_str = match expr_opt with
                    | Some expr -> 
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None ->
                        match last_expr with
                        | Some expr ->
                            ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
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
                ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level ctx body ^ "; }\n\n"
      in
      body_str
  | DModule (_, s) -> (
      "namespace " ^ cxx_deal_module s ^ " {\n\n"
  )
  | SExport (_, _) ->
      ""
  | SImport (_, import) -> Wrapper.toml_get_codegen_import import
  | SImportAs (_, import, alias) -> Wrapper.toml_get_codegen_importas import alias
  | SImportHere (_, import) -> Wrapper.toml_get_codegen_importhere import
  | DTest(_, _, _) -> ""


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

let generate_test indent_level defs ctx = 
    let ind = indent indent_level in
    let ind_inner = indent (indent_level + 1) in
    let header = "#include <mvp_builtin.h>\n" in
    let body = ref "" in 
    let found = ref false in
    List.iter (fun def ->
      match def with
      | DTest (_, name, expr) ->
      let param_strs = ["";] in
      let ret_type = "mvp_builtin_int" in
      let func_signature = ret_type ^ " " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" in
      let body_str = match expr with
        | EBlock (_, stmts, expr_opt) ->
            let stmt_strs, expr_str = 
                let stmt_strs, last_expr = 
                match List.rev stmts with
                | SExpr (_, expr) :: rest ->
                    let rev_stmts = List.rev rest in
                    (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) rev_stmts, Some expr)
                | _ ->
                    (List.map (cxx_stmt_of_stmt (indent_level + 1) ctx) stmts, None)
                in
                let expr_str = match expr_opt with
                | Some expr -> 
                    ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                | None ->
                    match last_expr with
                    | Some expr ->
                        ind_inner ^ "return " ^ cxx_expr_of_expr (indent_level + 1) ctx expr ^ ";\n"
                    | None -> ""
                in
                (stmt_strs, expr_str)
            in
            found := true;
            ind ^ func_signature ^ " {\n" ^ 
            String.concat "" stmt_strs ^ expr_str ^ 
            ind ^ "}\n\n"
        | _ -> 
            ind ^ func_signature ^ " { return " ^ cxx_expr_of_expr indent_level ctx expr ^ "; }\n\n"
      in
      body := ((!body) ^ body_str)
      | _ -> ()
    ) defs;
    if not (!found) then "" else
    header ^ !body ^ "int main() {\n" ^
      "mvp_builtin_int passed = 0, failed = 0;\n" ^
      String.concat "" (List.map( fun def -> (
        match def with
        | DTest (_, name, _) -> (
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

let generate_header defs =
  let symbol_table = build_symbol_table defs in
  let header_guard = "#pragma once\n\n" in
  let includes = "#include <mvp_builtin.h>\n\n" in
  
  let rec collect_exported_decls defs current_modules = 
    match defs with
    | [] -> ""
    | def :: rest ->
        match def with
        | DModule (_, module_name) ->
            let new_modules = current_modules @ [module_name] in
            let module_parts = String.split_on_char '.' module_name in
            let module_parts = if get_head module_parts = Some "std" then
              "mvp_std" :: (module_parts |> List.tl)
            else if get_head module_parts = Some "main" then
              let res = "mvp_main" :: (module_parts |> List.tl) in
              res
            else
              module_parts in
            let module_start = List.fold_left (fun acc part -> acc ^ "namespace " ^ part ^ " {\n\n") "" module_parts in
            let inner_content = collect_exported_decls rest new_modules in
            let module_end = List.fold_left (fun acc _ -> acc ^ "}\n\n") "" module_parts in
            module_start ^ inner_content ^ module_end
        | SExport (_, name) ->
            let struct_info = List.find_opt (fun (sname, _) -> sname = name) symbol_table.structs in
            let decl = match struct_info with
              | Some (sname, fields) ->
                  "struct " ^ sname ^ " {\n" ^
                  List.fold_left (fun acc (fname, ftyp) ->
                      acc ^ "  " ^ cxx_type_of_typ ftyp ^ " " ^ fname ^ ";\n"
                    ) "" fields ^
                  "};\n\n"
              | None ->
                  let func_info = List.find_opt (fun (fname, _, _, _) -> fname = name) symbol_table.functions in
                  match func_info with
                  | Some (fname, params, ret_typ_opt, _) ->
                      cxx_func_declaration fname params ret_typ_opt
                  | None ->
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
    "" 
  else
    header_guard ^ includes ^ exported_decls ^ footer

let build_ir _fpath defs = 
  let header_content = generate_header defs in
  let ctx = () in
  let header = "#include <iostream>\n" ^
  "#include <string>\n" ^
  "#include <vector>\n" ^
  "#include <cstdint>\n" ^
  "#include <mvp_builtin.h>\n" ^
  "\n" ^
  "\nusing namespace std;\n\n" in
  
  let rec generate_with_scope ctx current_module main_functions includes defs_str defs = 
    match defs with
    | [] -> (includes, defs_str, main_functions)
    | def :: rest -> 
        match def with
        | DModule (_, module_name) ->
            (* Start nested module scopes *)
            let module_parts = String.split_on_char '.' module_name in
            let module_parts = if get_head module_parts = Some "std" then
              "mvp_std" :: (module_parts |> List.tl)
            else if get_head module_parts = Some "main" then
              let res = "mvp_main" :: (module_parts |> List.tl) in
              res
            else
              module_parts in
            let module_start = List.fold_left (fun acc part -> acc ^ "namespace " ^ part ^ " {\n\n") "" module_parts in
            let inner_includes, inner_content, new_main_functions = generate_with_scope ctx (Some module_name) main_functions "" "" rest in
            let module_end = List.fold_left (fun acc _ -> acc ^ "}\n\n") "" module_parts in
            (includes ^ inner_includes, defs_str ^ module_start ^ inner_content ^ module_end, new_main_functions)
        | DFunc (loc, "main", params, _, body) ->
            let mvp_main_def = DFunc (loc, "main", params, None, body) in
            let mvp_main_str = cxx_def_of_def 0 ctx mvp_main_def in
            
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
            
            generate_with_scope ctx current_module (main_functions ^ global_main) includes (defs_str ^ mvp_main_str) rest
        | SImport (_, _) ->
            let import_str = cxx_def_of_def 0 ctx def in
            generate_with_scope ctx current_module main_functions (includes ^ import_str) defs_str rest
        | SImportAs (_, _, _) ->
            let import_str = cxx_def_of_def 0 ctx def in
            generate_with_scope ctx current_module main_functions (includes ^ import_str) defs_str rest
        | SImportHere (_, _) ->
            let import_str = cxx_def_of_def 0 ctx def in
            generate_with_scope ctx current_module main_functions (includes ^ import_str) defs_str rest
        | _ ->
            let def_str = cxx_def_of_def 0 ctx def in
            generate_with_scope ctx current_module main_functions includes (defs_str ^ def_str) rest
  in
  
  let includes, module_content, main_functions = generate_with_scope ctx None "" "" "" defs in
  let program = header ^ includes ^ module_content ^ main_functions ^ "\n" in
  let test = generate_test 0 defs ctx in
  [program;header_content;test]
