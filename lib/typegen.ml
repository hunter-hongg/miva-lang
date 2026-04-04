open Ast 

module StringMap = Map.Make(String)

let rec to_string typ = 
  match typ with
  | TInt -> "int"
  | TBool -> "bool"
  | TFloat32 -> "float32"
  | TFloat64 -> "float64"
  | TChar -> "char"
  | TString -> "string"
  | TPtr t -> "ptr of " ^ to_string t
  | TBox t -> "box of " ^ to_string t
  | TArray t -> "array of " ^ to_string t
  | TNull -> "null"
  | TInvalid -> "invalid type"
  | TStruct (n, fields) -> "struct " ^ n ^ String.concat ", " (List.map (fun f -> (
    let (n, t) = f in 
    n ^ " " ^ to_string t
  )) fields)

type safety = 
  | Safe 
  | Trusted
  | Unsafe

type symbols = {
  func: (string * param list * typ option * safety) list;
  structs: (string * (string * typ) list) list;
}

type vars = {
  vars: (string * typ) list;
  t : unit;
}

let empty_symbols = {
  func = [];
  structs = [];
}

let empty_vars = {
  vars = [];
  t = ()
}

let lookup_vars env vars = 
  let typ = ref TInvalid in
  List.iter (fun (n, t) -> (
    if n = vars then 
      if !typ = TInvalid then
        typ := t
  )) env.vars;
  !typ

let build_symbols start_file = 
  let ast = Wrapper.parse_input_file start_file in 
  let sym_t = Symbol_table.build_symbol_table ast in
  let deps = sym_t.files in 
  let maps = ref StringMap.empty in
  let sym_ret = ref empty_symbols in
  List.iter (fun d -> (
    match d with 
    | SImport(_, f) -> (
      match Tomler.toml_from_import f with 
      | Some f -> (
        let ast = Wrapper.parse_input_file f in 
        let sym = Symbol_table.build_symbol_table ast in
        let modn = sym.module_name in
        maps := StringMap.add f [modn;] (!maps)
      )
      | None -> ()
    )
    | SImportAs (_, f, a) -> (
      match Tomler.toml_from_import f with 
      | Some f -> (
        let ast = Wrapper.parse_input_file f in 
        let sym = Symbol_table.build_symbol_table ast in
        let modn = sym.module_name in
        maps := StringMap.add f [modn; a] (!maps)
      )
      | None -> ()
    )
    | SImportHere (_, f) -> (
      match Tomler.toml_from_import f with 
      | Some f -> (
        let ast = Wrapper.parse_input_file f in 
        let sym = Symbol_table.build_symbol_table ast in
        let modn = sym.module_name in
        maps := StringMap.add f [modn; ""] (!maps)
      )
      | None -> ()
    )
    | _ -> ()
  )) ast;
  maps := StringMap.add start_file [""] (!maps);
  let build_sym defs = 
    let mods = match StringMap.find_opt start_file (!maps) with 
    | Some mods -> List.map (fun m -> (
      if m <> "" then 
        m ^ "."
      else 
        m
    )) mods
    | None -> []
    in
    let do_func name params return safety = 
      List.iter (fun m -> (
        sym_ret := {
          !sym_ret with 
          func = (m ^ name, params, return, safety) :: !sym_ret.func
        }
      )) mods 
    in
    let do_stru name fields = 
      List.iter (fun m -> (
        sym_ret := {
          !sym_ret with 
          structs = (m ^ name, fields) :: !sym_ret.structs
        }
      )) mods 
    in
    List.iter (function 
    | DFunc (_, name, params, return, _) -> (
      do_func name params return Safe
    )
    | DFuncUnsafe (_, name, params, return, _) -> (
      do_func name params return Unsafe
    )
    | DFuncTrusted (_, name, params, return, _) -> (
      do_func name params return Trusted
    )
    | DCFuncUnsafe (_, name, params, return, _) -> (
      do_func name params return Unsafe
    )
    | DStruct (_, name, fields) -> (
      do_stru name fields
    )
    | _ -> ()
    ) defs
  in
  List.iter (fun f -> (
    let ast = Wrapper.parse_input_file f in 
    build_sym ast
  )) (start_file :: deps);
  (!sym_ret)

let lookup_struct sym name = 
  let res = List.filter_map (fun s -> (
    let (n, fields) = s in
    if n = name then Some fields else None
  )) sym.structs in
  if res = [] then 
    None
  else
    Some (List.hd res)

let lookup_function sym name = 
  let res = List.filter_map (fun f -> (
    let (n, params, return, safety) = f in
    if n = name then Some (params, return, safety) else None
  )) sym.func in
  if res = [] then 
    None
  else
    Some (List.hd res)

let rec type_of_expr sym env = function 
  | EInt (_, _) -> TInt 
  | EBool (_, _) -> TBool
  | EFloat (_, _) -> TFloat64
  | EChar (_, _) -> TChar
  | EString (_, _) -> TString
  | EStructLit (_, n, _) -> (
    match lookup_struct sym n with 
    | Some fields -> (
      TStruct (n, fields)
    )
    | None -> TInvalid
  )
  | ECall (_, name, _) -> (
    match lookup_function sym name with 
    | Some (_, return, _) -> (
      match return with 
      | Some t -> t
      | None -> TNull
    )
    | None -> TInvalid
  )
  | EBinOp (_, o, e1, _) -> (match o with 
    | Add | Sub | Mul -> type_of_expr sym env e1
    | Eq | Neq -> TBool
  )
  | EAddr (_, e) -> TPtr (type_of_expr sym env e)
  | EDeref (_, e) -> (
    match type_of_expr sym env e with 
    | TPtr t -> t
    | _ -> TInvalid
  )
  | EArrayLit (_, es) -> (
    let es = List.map (fun e -> type_of_expr sym env e) es in
    TArray (List.hd es)
  )
  | EVoid _ -> TNull
  | EIf (_, _, e1, _) -> type_of_expr sym env e1
  | EChoose (_, _, el, _) -> (
    let el = List.map (fun (_, e) -> type_of_expr sym env e) el in
    List.hd el
  )
  | ECast (_, _, t) -> t
  | EBlock (_, _, e) -> (
    match e with 
    | Some e -> type_of_expr sym env e
    | None -> TNull
  )
  | EMacro (_, _, _) -> TInvalid
  | EWhile (_, _, e) -> type_of_expr sym env e
  | ELoop (_, e) -> type_of_expr sym env e
  | EFor (_, _, _, e) -> type_of_expr sym env e
  | EVar (_, name)
  | EMove (_, name)
  | EClone (_, name)
   -> (
    lookup_vars env name
  )
  | EFieldAccess (_, e, f) -> (
    let t = type_of_expr sym env e in 
    match t with 
    | TStruct (_, fields) -> (
      let typ_ = ref TInvalid in
      List.iter (fun (n, t) -> (
        if n = f then typ_ := t else ()
      )) fields;
      !typ_
    )
    | _ -> TInvalid
  )

let fail_if errs loc cond msg = 
  if cond then 
    errs := !errs @ [Util.format_loc loc ^ ":" ^ msg]

let rec make_sure_expr sym env expr = 
  let errs = ref [] in
  let _ = match expr with 
  | EInt (_, _)
  | EBool (_, _)
  | EFloat (_, _)
  | EChar (_, _)
  | EString (_, _) 
  | EVoid _
  | EMacro (_, _, _)
  -> ()
  | EStructLit (_, _, fields) -> (
    List.iter (fun (_, e) -> (
      errs := !errs @ make_sure_expr sym env e
    )) fields
  )
  | EBinOp (loc, _, e1, e2) -> (
    errs := !errs @ make_sure_expr sym env e1;
    errs := !errs @ make_sure_expr sym env e2;
    let t1 = type_of_expr sym env e1 in
    let t2 = type_of_expr sym env e2 in
    fail_if errs loc (t1 <> t2) "binary operator type mismatch";
    fail_if errs loc (t1 = TNull) "cannot add null";
    fail_if errs loc (t2 = TNull) "cannot add null";
    fail_if errs loc (t1 = TInvalid) "type invalid";
    fail_if errs loc (t2 = TInvalid) "type invalid";
  )
  | ECast (_, e, _) -> errs := !errs @ make_sure_expr sym env e 
  | EIf (loc, cond, e1, e2) -> (
    errs := !errs @ make_sure_expr sym env cond;
    errs := !errs @ make_sure_expr sym env e1;
    match e2 with 
    | Some e2 -> errs := !errs @ make_sure_expr sym env e2;
    | None -> ();
    let t1 = type_of_expr sym env e1 in
    let t2 = match e2 with 
    | Some e2 -> type_of_expr sym env e2
    | None -> TNull
    in
    fail_if errs loc (type_of_expr sym env cond <> TBool) "if condition type be bool";
    fail_if errs loc (t1 <> t2) "if type mismatch";
    fail_if errs loc (t1 = TInvalid) "type invalid";
    fail_if errs loc (t2 = TInvalid) "type invalid";
  )
  | EArrayLit (loc, es) -> (
    let es = List.map (fun e -> (
      errs := !errs @ make_sure_expr sym env e;
      let t = type_of_expr sym env e in 
      fail_if errs loc (t = TInvalid) "type invalid";
      t
    )) es in
    let t = List.hd es in
    fail_if errs loc (List.for_all (fun e -> e = t) es) "array type mismatch";
  )
  | EAddr (_, e) -> (
    errs := !errs @ make_sure_expr sym env e;
  )
  | EDeref (loc, e) -> (
    errs := !errs @ make_sure_expr sym env e;
    let t = type_of_expr sym env e in 
    fail_if errs loc (t = TInvalid) "type invalid";
  )
  | EVar (loc, name)
  | EMove (loc, name)
  | EClone (loc, name)
   -> (
    let t = lookup_vars env name in 
    fail_if errs loc (t = TInvalid) "type invalid";
   )
   | EFieldAccess (loc, _, _) as e
   -> (
    let t = type_of_expr sym env e in 
    fail_if errs loc (t = TInvalid) "type invalid";
   )
  | ECall (loc, name, el) -> (
    if not (List.mem name Global.builtin_functions) then
      match lookup_function sym name with 
      | Some f -> (
        let pl, _, _ = f in
        let pl = List.map (fun p -> (
          match p with 
          | PRef (_, t)
          | POwn (_, t)
          -> t
        )) pl in
        List.iter (fun e -> (
          errs := !errs @ make_sure_expr sym env e;
        )) el;
        fail_if errs loc (List.length el <> List.length pl) "argument count mismatch";
        if List.length el = List.length pl then (
          for i = 0 to List.length el - 1 do (
            let e = List.nth el i in
            let t = type_of_expr sym env e in
            fail_if errs loc (t <> List.nth pl i) "argument type mismatch";
          )
          done
        )
      )
      | None -> fail_if errs loc (true) "function not found"
    else
      ()
  )
  | ELoop (_, e) -> (
    errs := !errs @ make_sure_expr sym env e;
  )
  | EFor (_, _, e1, e2) -> (
    errs := !errs @ make_sure_expr sym env e1;
    errs := !errs @ make_sure_expr sym env e2;
  )
  | EWhile (loc, cond, e) -> (
    errs := !errs @ make_sure_expr sym env cond;
    errs := !errs @ make_sure_expr sym env e;
    fail_if errs loc (type_of_expr sym env cond <> TBool) "while condition type be bool";
  )
  | EBlock (_, _, _) as e -> (
    let er, _ = make_sure_block sym env e in
    errs := !errs @ er;
  )
  | EChoose (loc, e, el, eo) -> (
    errs := !errs @ make_sure_expr sym env e;
    let esl = List.map (fun (e1, e) -> (
      errs := !errs @ make_sure_expr sym env e;
      errs := !errs @ make_sure_expr sym env e1;
      type_of_expr sym env e
    )) el in
    let tf = List.hd esl in
    fail_if errs loc (List.for_all (fun e -> e = tf) esl) "choose type mismatch";
    match eo with 
    | Some e -> (
      errs := !errs @ make_sure_expr sym env e;
      let t = type_of_expr sym env e in
      fail_if errs loc (t <> tf) "choose type mismatch";
    )
    | None -> ()
  )
  in
  !errs

and make_sure_block sym env expr = 
  let errs = ref [] in
  let env = ref env in
  let t = match expr with 
  | EBlock (_, sl, e) -> (
    List.iter (fun s -> (
      match s with 
      | SLet (_, _, name, e) -> (
        env := {
          !env with 
          vars = !env.vars @ [(name, type_of_expr sym !env e)]
        };
        errs := !errs @ make_sure_expr sym !env e;
      )
      | SAssign (loc, name, e) -> (
        errs := !errs @ make_sure_expr sym !env e;
        let t = lookup_vars !env name in
        fail_if errs loc (t = TInvalid) "type invalid";
        fail_if errs loc (t <> type_of_expr sym !env e) "assignment type mismatch";
      )
      | SReturn (_, _) -> ()
      | SExpr (_, e) -> (
        let er, _ = make_sure_block sym !env e in
        errs := !errs @ er;
      )
    )) sl;
    match e with 
    | Some e -> (
      let er, _ = make_sure_block sym !env e in
      errs := !errs @ er;
      let t = type_of_expr sym !env e in
      t
    )
    | None -> TNull
  )
  | e -> (
    errs := !errs @ make_sure_expr sym !env e;
    type_of_expr sym !env e
  ) in
  !errs, t

let type_check_def sym defs = 
  let errs = ref [] in 
  let _ = match defs with
  | DFunc (_, _, p, _, e)
  | DFuncTrusted (_, _, p, _, e)
  | DFuncUnsafe (_, _, p, _, e)
   -> (
    let env = ref empty_vars in 
    List.iter (fun p -> (
      let (n, t) = match p with 
      | PRef (n, t) -> (n, t)
      | POwn (n, t) -> (n, t)
      in
      env := {
        !env with
        vars = !env.vars @ [(n, t)]
      }
    )) p;
    let er, _ = make_sure_block sym !env e in
    errs := !errs @ er;
  )
  | _ -> () 
  in
  !errs

let check_defs file defs = 
  let errs = ref [] in
  let sym = build_symbols file in
  List.iter (fun d -> (
    errs := !errs @ type_check_def sym d
  )) defs;
  !errs
