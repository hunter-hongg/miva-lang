open Global 
open Ast

let get_warnings defs = 
  let warnings = ref [] in
  let symt = Symbol_table.build_symbol_table defs in 
  let modname = symt.module_name in
  let rec check_expr expr = 
    match expr with
    | ECall (loc, name, args) -> 
      (match (deperecated name modname) with
       | Some msg -> 
         let warning_msg = Printf.sprintf "Warning:%d:%d:%s" loc.line loc.col msg in
         warnings := warning_msg :: !warnings
       | None -> ());
      List.iter check_expr args
    | EMacro (_, _, _) -> 
      ()
    | EBinOp (_, _, e1, e2) -> 
      check_expr e1;
      check_expr e2
    | EIf (_, cond, then_expr, else_expr) -> 
      check_expr cond;
      check_expr then_expr;
      Option.iter check_expr else_expr
    | EChoose (_, cond, cases, else_expr) -> 
      check_expr cond;
      List.iter (fun (cond, body) -> check_expr cond; check_expr body) cases;
      Option.iter check_expr else_expr
    | EFieldAccess (_, e, _) -> check_expr e
    | EStructLit (_, _, fields) -> 
      List.iter (fun (_, e) -> check_expr e) fields
    | EBlock (_, stmts, ret_expr) -> 
      List.iter check_stmt stmts;
      Option.iter check_expr ret_expr
    | EArrayLit (_, elems) -> 
      List.iter check_expr elems
    | ECast (_, e, _) -> check_expr e
    | EWhile (_, cond, body) -> 
      check_expr cond;
      check_expr body
    | ELoop (_, body) -> check_expr body
    | EFor (_, _, range, body) -> 
      check_expr range;
      check_expr body
    | EAddr (_, e) -> check_expr e
    | EDeref (_, e) -> check_expr e
    | _ -> ()
  and check_stmt stmt = 
    match stmt with
    | SLet (loc, _, name, e) -> (
      match Util.check_snake loc name "var" with
      | Some msg -> warnings := msg :: !warnings
      | None -> ();
      check_expr e;
    )
    | SAssign (_, _, e) -> check_expr e
    | SReturn (_, e) -> check_expr e
    | SExpr (_, e) -> check_expr e
  in
  List.iter (function def ->
    match def with
    | DFunc (loc, name, _, _, expr)
    | DFuncTrusted (loc, name, _, _, expr)
    | DFuncUnsafe (loc, name, _, _, expr)
     -> (
      match Util.check_snake loc name "function" with
      | Some msg -> warnings := msg :: !warnings
      | None -> ();
      check_expr expr
    )
    | DModule (loc, name) -> (
      match Util.check_all_lower loc name "module" with
      | Some msg -> warnings := msg :: !warnings
      | None -> ();
    )
    | _ -> ()
  ) defs;
  !warnings
