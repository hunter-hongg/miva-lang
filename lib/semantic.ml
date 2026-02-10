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

let rec is_copy_type types t =
  match t with
  | TInt | TBool | TFloat32 | TFloat64 | TChar -> true
  | TString | TArray _ -> false
  | TStruct (name, _) ->
      try
        let fields = Env.find name types in
        List.for_all (fun (_, ft) -> is_copy_type types ft) fields
      with Not_found -> false

let rec check_expr ctx e =
  let err msg = failwith msg in
  let mark_moved name =
    if Env.mem name ctx.vars then (
      let info = Env.find name ctx.vars in
      if info.state = `Moved then err ("Use of moved value: " ^ name);
      let new_vars = Env.add name {info with state = `Moved} ctx.vars in
      ctx.vars <- new_vars
    )
  in
  match e with
  | EVar name ->
      if Env.mem name ctx.vars then (
        let info = Env.find name ctx.vars in
        if info.state = `Moved then err ("Use after move: " ^ name)
      )
  | EMove name -> mark_moved name
  | EClone name ->
      if Env.mem name ctx.vars then (
        let info = Env.find name ctx.vars in
        if not (is_copy_type ctx.types info.typ) then
          err ("Cannot clone non-Copy type: " ^ name);
        if info.state = `Moved then err ("Use of moved value: " ^ name)
      )
  | ECall (_, args) -> List.iter (check_expr ctx) args
  | EStructLit (_, inits) -> List.iter (fun (_, e) -> check_expr ctx e) inits
  | EFieldAccess (e, _) -> check_expr ctx e
  | EBinOp (_, e1, e2) -> check_expr ctx e1; check_expr ctx e2
  | EIf (cond, t, eopt) ->
      check_expr ctx cond;
      check_expr ctx t;
      Option.iter (check_expr ctx) eopt
  | ECast (e, _) -> check_expr ctx e
  | EBlock (stmts, expr_opt) ->
      (* 保存当前变量环境，用于块结束后恢复 *)
      let saved_vars = ctx.vars in
      (* 检查块中的所有语句 *)
      List.iter (fun stmt -> match stmt with
          | SLet (is_mutable, name, e) ->
              (* 首先检查右侧表达式 *)
              check_expr ctx e;
              (* 为变量分配一个默认类型，实际应用中应该进行类型推导 *)
              let var_type = TInt (* 临时默认类型，后续需要类型推导 *) in
              (* 将新变量添加到环境中 *)
              ctx.vars <- Env.add name { typ = var_type; state = `Valid; is_mutable } ctx.vars
          | SAssign (name, e) ->
              (* 检查变量是否可变 *)
              if Env.mem name ctx.vars then (
                let var_info = Env.find name ctx.vars in
                if not var_info.is_mutable then err ("Cannot assign to immutable variable: " ^ name);
                if var_info.state = `Moved then err ("Use of moved value: " ^ name);
                (* 检查右侧表达式 *)
                check_expr ctx e;
                (* 更新变量状态 *)
                ctx.vars <- Env.add name {var_info with state = `Valid} ctx.vars
              )
          | SReturn e -> check_expr ctx e
          | SExpr e -> check_expr ctx e
        ) stmts;
      (* 检查块中的最后表达式 *)
      Option.iter (check_expr ctx) expr_opt;
      (* 恢复保存的变量环境 *)
      ctx.vars <- saved_vars
  | _ -> ()

let check_program defs =
  let types = List.fold_left (fun acc -> function
      | DStruct (name, fields) -> Env.add name fields acc
      | _ -> acc
    ) Env.empty defs in

  List.iter (function
      | DFunc (_, params, _, body) ->
          let vars = List.fold_left (fun vars p ->
              let (pname, pty) = match p with
                | PRef (n, t) | POwn (n, t) -> (n, t) in
              Env.add pname { typ = pty; state = `Valid; is_mutable = false } vars
            ) Env.empty params in
          let ctx = { types; vars } in
          check_expr ctx body
      | DStruct (_, _) -> ()
    ) defs