[@@@ocaml.warning "-27"]
open Ast
open Wrapper
open Util

module Env = Map.Make(String)

type var_info = {
  typ: typ;
  mutable state: [`Valid | `Moved];
  is_mutable: bool;
  is_ref_param: bool;
}

type context = {
  types: (string * typ) list Env.t; 
  mutable vars: var_info Env.t;
}

let rec is_copy_type types t =
  match t with
  | TInt | TBool | TFloat32 | TFloat64 | TChar -> true
  | TString | TArray _ | TPtr _ | TBox _ | TNull | TInvalid -> false
  | TStruct (name, _) -> 
      try
        let fields = Env.find name types in
        List.for_all (fun (_, ft) -> is_copy_type types ft) fields
      with Not_found -> false

let rec check_expr ctx symbol_table e =
  let errs = ref [] in
  let err loc msg = errs := !errs @ [format_loc loc ^ ":" ^ msg] in
  let mark_moved loc name =
    if Env.mem name ctx.vars then (
      let info = Env.find name ctx.vars in
      if info.state = `Moved then err loc ("use of moved value: " ^ name);
      if info.is_ref_param then err loc ("cannot move ref parameter: " ^ name);
      let new_vars = Env.add name {info with state = `Moved} ctx.vars in
      ctx.vars <- new_vars
    )
  in
  let _ = match e with
  | EVar (loc, name) ->
    if Env.mem name ctx.vars then (
      let info = Env.find name ctx.vars in
      if info.state = `Moved then err loc ("use after moving" ^ name)
    ) else (
      errs := !errs @ [(format_loc loc) ^ ":" ^ "variable '" ^ name ^ "' not found"]
    )
  | EMove (loc, name) -> (
    if Env.mem name ctx.vars then (
      mark_moved loc name
    ) else (
      errs := !errs @ [(format_loc loc) ^ ":" ^ "variable '" ^ name ^ "' not found"]
    )
  )
  | EClone (loc, name) ->
    if Env.mem name ctx.vars then (
      let info = Env.find name ctx.vars in
      if info.state = `Moved then err loc ("use of moved value: " ^ name)
    ) else (
      errs := !errs @ [(format_loc loc) ^ ":" ^ "variable '" ^ name ^ "' not found"]
    )
  | ECall (loc, name, args) -> 
      (match Symbol_table.get_function_safety name symbol_table with
      | Some Symbol_table.Unsafe ->
          err loc ("cannot call unsafe function '" ^ name ^ "' from safe function")
      | Some Symbol_table.Trusted ->
          () 
      | Some Symbol_table.Safe ->
          () 
      | None ->
          if String.starts_with ~prefix:"ffi." name then 
            err loc ("cannot call unsafe ffi function '" ^ name ^ "' from safe function")
          else
            err loc ("unknown function: " ^ name)
      );
      List.iter (
        fun e -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
      ) args
  | EStructLit (loc, _, inits) -> List.iter (
    fun (_, e) -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
  ) inits
  | EArrayLit (loc, elems) -> List.iter (
    fun e -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
  ) elems
  | EFieldAccess (loc, e, _) -> errs := !errs @ (check_expr ctx symbol_table e)
  | EBinOp (loc, _, e1, e2) -> errs := !errs @ (check_expr ctx symbol_table e1) @ (check_expr ctx symbol_table e2)
  | EIf (loc, cond, t, eopt) ->
      let errsc = check_expr ctx symbol_table cond in
      errs := !errs @ errsc;
      
      let vars_before = ctx.vars in
      
      let errsc = check_expr ctx symbol_table t in errs := !errs @ errsc;
      let vars_after_then = ctx.vars in
      
      ctx.vars <- vars_before;
      let vars_after_else = match eopt with
        | Some e -> 
            let errsc = check_expr ctx symbol_table e in
            errs := !errs @ errsc;
            ctx.vars
        | None -> vars_before
      in
      
      let merged_vars = Env.merge (fun _ var_info_opt var_info_opt' ->
          match var_info_opt, var_info_opt' with
          | Some info1, Some info2 -> 
              if info1.state = `Moved || info2.state = `Moved then
                Some {info1 with state = `Moved}
              else
                Some info1
          | Some info, None | None, Some info ->
              Some info
          | None, None -> None
        ) vars_after_then vars_after_else in
      
      ctx.vars <- merged_vars
  | ECast (loc, e, _) -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
  | EChoose (loc, var_expr, when_branches, otherwise_opt) ->
      if otherwise_opt = None then
        err loc "choose expression must have an otherwise branch";
      
      let errsc = check_expr ctx symbol_table var_expr in errs := !errs @ errsc;
      
      let vars_before = ctx.vars in
      
      let branch_vars = List.map (fun (when_val, when_body) ->
          ctx.vars <- vars_before;
          let errsc = check_expr ctx symbol_table when_val in errs := !errs @ errsc;
          let errsc = check_expr ctx symbol_table when_body in errs := !errs @ errsc;
          ctx.vars
        ) when_branches in
      
      ctx.vars <- vars_before;
      let otherwise_vars = match otherwise_opt with
        | Some otherwise_body ->
            let errsc = check_expr ctx symbol_table otherwise_body in errs := !errs @ errsc;
            ctx.vars
        | None -> vars_before
      in
      
      let all_branch_vars = branch_vars @ [otherwise_vars] in
      
      let merged_vars = List.fold_left (fun acc_vars branch_vars ->
          Env.merge (fun _ var_info_opt branch_var_info_opt ->
              match var_info_opt, branch_var_info_opt with
              | Some info1, Some info2 -> 
                  if info1.state = `Moved || info2.state = `Moved then
                    Some {info1 with state = `Moved}
                  else
                    Some info1
              | Some info, None | None, Some info ->
                  Some info
              | None, None -> None
            ) acc_vars branch_vars
        ) vars_before all_branch_vars in
      
      ctx.vars <- merged_vars
  | EBlock (_, stmts, expr_opt) ->
      let saved_vars = ctx.vars in
      List.iter (fun stmt -> match stmt with
          | SLet (_, is_mutable, name, e) ->
              let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc;
              let var_type = TInt in
              ctx.vars <- Env.add name { typ = var_type; state = `Valid; is_mutable; is_ref_param = false } ctx.vars
          | SAssign (loc, name, e) ->
              if Env.mem name ctx.vars then (
                let var_info = Env.find name ctx.vars in
                if not var_info.is_mutable then err loc ("cannot assign to immutable variable: " ^ name);
                if var_info.state = `Moved then err loc ("use of moved value: " ^ name);
                let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc;
                ctx.vars <- Env.add name {var_info with state = `Valid} ctx.vars
              )
          | SReturn (loc, e) -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
          | SExpr (loc, e) -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
        ) stmts;
      Option.iter (
        fun e -> let errsc = check_expr ctx symbol_table e in errs := !errs @ errsc
      ) expr_opt;
      
      let propagated_vars = Env.merge (fun _name saved_info_opt current_info_opt ->
          match saved_info_opt, current_info_opt with
          | Some saved_info, Some current_info ->
              if current_info.state = `Moved then
                Some {saved_info with state = `Moved}
              else
                Some saved_info
          | Some saved_info, None ->
              Some saved_info
          | None, Some _ ->
              None
          | None, None -> None
        ) saved_vars ctx.vars in
      
      ctx.vars <- propagated_vars
  | EDeref (loc, e) -> err loc "cannot dereference a ptr in a safe function."
  | _ -> () in
  !errs

let check_program defs =
  let symbol_table = Symbol_table.build_symbol_table defs in
  let errs = ref [] in

  let alias_module = List.filter_map((fun d -> (
  match d with 
  | SImportAs (loc, import, alias) -> (
    if String.starts_with ~prefix:"c:" import then (
      errs := !errs @ [(format_loc loc) ^ ":cannot import c file with an alias"];
      None
    ) else (
      (Wrapper.toml_deal_ias import alias)
    ))

  | _ -> None
  ))) defs in

  let funct = ref (symbol_table.functions) in
  let _ = List.iter (fun def -> (
    funct := !funct @ [(def, [], None, Safe)];
    ()
  )) Global.builtin_functions in
  let here_module = List.filter_map (fun d -> (
    match d with 
    | SImportHere (_, import) -> (
        Wrapper.toml_get_mod_std import
      )
    | _ -> None
  )) defs in

  List.iter (fun file -> (
    let defsn = parse_input_file file in 
    let symbt = Symbol_table.build_symbol_table defsn in
  
    List.iter (fun fn -> (
      match fn with 
      | (name, params, return_typ, safety) -> (
        let nmod = Wrapper.deal_std symbt.module_name in
        if (
          List.filter (fun x -> get_head x = Some nmod) alias_module <> []
        ) then (
          let nfn = (nmod ^ "." ^ name, params, return_typ, safety) in
          let nfn2 = ((
            let h = (
              match (List.filter (fun x -> get_head x = Some nmod) alias_module) |> get_head with 
              | Some h -> h
              | None -> []
            ) in 
            match h with 
            | [raw; alias] -> (
              alias
            )
            | _ -> ""
          ) ^ "." ^ name, params, return_typ, safety) in
          funct := !funct @ [nfn; nfn2]
        ) else (
          if (
            List.filter (fun x -> x = nmod) here_module <> []
          ) then (
            let nfn = (nmod ^ "." ^ name, params, return_typ, safety) in
            let nfn2 = (name, params, return_typ, safety) in
            funct := !funct @ [nfn; nfn2]
          ) else (
            let nfn = (nmod ^ "." ^ name, params, return_typ, safety) in
            funct := !funct @ [nfn]
          )
        ) 
       )
    )) symbt.exported_functions;
    ()
  )) symbol_table.files; 
  let symbol_table = {symbol_table with functions = !funct} in
  let module_decls = ref [] in
  let has_non_module_decl = ref false in
  let has_module_decl = ref false in
  
  List.iter (function
      | DModule (loc, name) ->
          if !has_non_module_decl then
            errs := !errs @ [ 
              (format_loc loc) ^ ":module declaration 'module " ^ name ^ 
                "' must be at the top of the file, before any other declarations"];
          
          if List.mem name !module_decls then
            errs := !errs @ [
              (format_loc loc) ^ ":duplicate module declaration: 'module " ^ name ^ "'"];
          
          module_decls := name :: !module_decls;
          has_module_decl := true
      | _ ->
          has_non_module_decl := true
    ) defs;
  
  if not !has_module_decl then
    errs := !errs @ [
      "0:0:program must have one module declaration"];
  
  let types = List.fold_left (fun acc -> function
    | DStruct (_, name, fields) -> Env.add name fields acc
    | _ -> acc
  ) Env.empty defs in

  List.iter (function
    | DFunc (_, _, params, _, body) ->
        let vars = List.fold_left (fun vars p ->
            match p with
            | PRef (n, t) -> 
                Env.add n { typ = t; state = `Valid; is_mutable = false; is_ref_param = true } vars
            | POwn (n, t) -> 
                Env.add n { typ = t; state = `Valid; is_mutable = false; is_ref_param = false } vars
          ) Env.empty params in
        let ctx = { types; vars } in
        let errsc = check_expr ctx symbol_table body in
        errs := !errs @ errsc
    | DFuncUnsafe (_, _, params, _, body) ->
        ()
    | DCFuncUnsafe (_, _, params, _, body) ->
        ()
    | DFuncTrusted (_, _, params, _, body) ->
        ()
    | DTest (_, _, body) ->
        let ctx = { types; vars = Env.empty } in
        let errsc = check_expr ctx symbol_table body in
        errs := !errs @ errsc
    | DStruct (_, _, _) -> ()
    | DModule _ -> ()  
    | _ -> ()
  ) defs;
  
  !errs