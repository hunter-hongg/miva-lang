open Ast
(* open Lexing
open Parsing *)

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let expand_macros defs = 
  let addf = ref [] in
  let rec print_basic_expr loc args = (
    let res = ref [
      SLet(
        loc, true, "s", EString (loc, "")
      );
    ] in 
    List.iter (
      fun e -> (
        res := !res @ [SAssign(
          loc, "s", 
          EBinOp(
            loc, 
            Add,
            EBinOp (
              loc, 
              Add,
              EMove(loc, "s"),
              ECall (
                loc, "string_from", [expand_expr e]
              )
            ),
            EString(loc, " ")
          )
        );]
    )) args;
    res := !res @ [
      SExpr(
        loc, ECall(
          loc, "print", [
            EVar(loc, "s");
          ]
        )
      );
    ];
    !res
  ) 
  and expand_expr = function
    | EMacro (loc, name, args) -> (
      match name with 
      | "include_str" -> (
        match args with
        | [EString (_, file)] -> (
          EString (loc, String.escaped (read_file file))
        )
        | _ -> failwith "include_str! macro expects a single string argument"
      )
      | "prints" -> (
        addf := SImport(loc, "std/str") :: !addf;
        let res = print_basic_expr loc args in
        EBlock(loc, res, None)
      )
      | "printlns" -> (
        addf := SImport(loc, "std/str") :: !addf;
        let res = print_basic_expr loc args in
        EBlock(loc, res @ [
          SExpr(
            loc, ECall(
              loc, "print", [
                EString(loc, "\\n");
              ]
            )
          )
        ], None)
      )
      | "assert" -> (
        match args with 
        | [e] -> (
          EBlock(loc, [
            SExpr(loc, 
              EIf(loc, 
                EBinOp(loc, Eq, expand_expr e, EBool(loc, false)),
                EBlock(loc, [
                  SExpr(loc, ECall(loc, "panic", [
                    EString(loc, "Assertion failed");
                  ]))
                ], None),
                None
              )
            )
          ], None)
        )
        | _ -> failwith "assert! macro expects a single expression argument"
      )
      | s -> (
        failwith ("Unknown macro: " ^ s)
      )
    )
    | EStructLit (loc, name, fields) -> 
        EStructLit (loc, name, List.map (fun (f, e) -> (f, expand_expr e)) fields)
    | EFieldAccess (loc, e, field) -> EFieldAccess (loc, expand_expr e, field)
    | EBinOp (loc, op, e1, e2) -> EBinOp (loc, op, expand_expr e1, expand_expr e2)
    | EIf (loc, cond, then_e, else_e) -> 
        EIf (loc, expand_expr cond, expand_expr then_e, Option.map expand_expr else_e)
    | EChoose (loc, cond, cases, else_e) -> 
        EChoose (loc, expand_expr cond, 
                List.map (fun (c, e) -> (expand_expr c, expand_expr e)) cases, 
                Option.map expand_expr else_e)
    | ECall (loc, name, args) -> ECall (loc, name, List.map expand_expr args)
    | ECast (loc, e, t) -> ECast (loc, expand_expr e, t)
    | EBlock (loc, stmts, ret_e) -> 
        EBlock (loc, List.map expand_stmt stmts, Option.map expand_expr ret_e)
    | EArrayLit (loc, es) -> EArrayLit (loc, List.map expand_expr es)
    | EAddr (loc, e) -> EAddr (loc, expand_expr e)
    | EDeref (loc, e) -> EDeref (loc, expand_expr e)
    | EFor (loc, var, range, body) -> 
        EFor (loc, var, expand_expr range, expand_expr body)
    | ELoop (loc, body) -> ELoop (loc, expand_expr body)
    | EWhile (loc, cond, body) -> EWhile (loc, expand_expr cond, expand_expr body)
    | e -> e  (* 其他所有表达式类型直接返回 *)
  and expand_stmt = function
    | SLet (loc, mut, name, e) -> SLet (loc, mut, name, expand_expr e)
    | SAssign (loc, name, e) -> SAssign (loc, name, expand_expr e)
    | SReturn (loc, e) -> SReturn (loc, expand_expr e)
    | SExpr (loc, e) -> SExpr (loc, expand_expr e)
    | SCIntro (loc, intro) -> SCIntro (loc, intro)
    | SEmpty loc -> SEmpty loc
  and expand_def = function
    | DFunc (loc, name, params, ret_typ, body) -> 
        DFunc (loc, name, params, ret_typ, expand_expr body)
    | DFuncUnsafe (loc, name, params, ret_typ, body) -> 
        DFuncUnsafe (loc, name, params, ret_typ, expand_expr body)
    | DFuncTrusted (loc, name, params, ret_typ, body) -> 
        DFuncTrusted (loc, name, params, ret_typ, expand_expr body)
    | DTest (loc, name, body) -> DTest (loc, name, expand_expr body)
    | d -> d  
  in
  let res = List.map expand_def defs in 
  match res with
  | first :: rest -> (
    first :: (!addf @ rest)
  )
  | [] -> !addf