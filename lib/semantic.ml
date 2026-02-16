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
      
      (* 悲观合并：保存分支前的变量状态 *)
      let vars_before = ctx.vars in
      
      (* 检查then分支 *)
      check_expr ctx t;
      let vars_after_then = ctx.vars in
      
      (* 恢复变量状态，检查else分支（如果有） *)
      ctx.vars <- vars_before;
      let vars_after_else = match eopt with
        | Some e -> 
            check_expr ctx e;
            ctx.vars
        | None -> vars_before
      in
      
      (* 悲观合并：如果变量在任一分支中被移动，则合并后状态为移动 *)
      let merged_vars = Env.merge (fun _ var_info_opt var_info_opt' ->
          match var_info_opt, var_info_opt' with
          | Some info1, Some info2 -> 
              (* 如果任一分支中变量状态为移动，则合并后为移动 *)
              if info1.state = `Moved || info2.state = `Moved then
                Some {info1 with state = `Moved}
              else
                Some info1
          | Some info, None | None, Some info ->
              (* 变量只在一个分支中存在，保持其状态 *)
              Some info
          | None, None -> None
        ) vars_after_then vars_after_else in
      
      ctx.vars <- merged_vars
  | ECast (e, _) -> check_expr ctx e
  | EChoose (var_expr, when_branches, otherwise_opt) ->
      (* 检查choose表达式：otherwise分支不可省略 *)
      if otherwise_opt = None then
        err "choose expression must have an otherwise branch";
      
      (* 检查变量表达式 *)
      check_expr ctx var_expr;
      
      (* 检查所有when分支 *)
      List.iter (fun (when_val, when_body) ->
          check_expr ctx when_val;
          check_expr ctx when_body
        ) when_branches;
      
      (* 检查otherwise分支 *)
      Option.iter (check_expr ctx) otherwise_opt
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
      
      (* 悲观合并：传播对外层变量的移动操作 *)
      (* 只保留外层作用域中存在的变量，并更新它们的状态 *)
      let propagated_vars = Env.merge (fun _name saved_info_opt current_info_opt ->
          match saved_info_opt, current_info_opt with
          | Some saved_info, Some current_info ->
              (* 变量在块内外都存在，如果块内移动了变量，则传播移动状态 *)
              if current_info.state = `Moved then
                Some {saved_info with state = `Moved}
              else
                Some saved_info
          | Some saved_info, None ->
              (* 变量只在块外存在，保持原状态 *)
              Some saved_info
          | None, Some _ ->
              (* 变量只在块内存在，不传播到外层 *)
              None
          | None, None -> None
        ) saved_vars ctx.vars in
      
      ctx.vars <- propagated_vars
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