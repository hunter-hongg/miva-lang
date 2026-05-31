open Ast

type json =
  | JNull
  | JBool of bool
  | JInt of int
  | JInt64 of int64
  | JFloat of float
  | JString of string
  | JArray of json list
  | JObject of (string * json) list

let json_string_of_json v =
  let buf = Buffer.create 256 in
  let rec emit v =
    match v with
    | JNull -> Buffer.add_string buf "null"
    | JBool b -> Buffer.add_string buf (if b then "true" else "false")
    | JInt n -> Buffer.add_string buf (string_of_int n)
    | JInt64 n -> Buffer.add_string buf (Int64.to_string n)
    | JFloat f ->
        let s = string_of_float f in
        let s = if String.contains s '.' || String.contains s 'e' || String.contains s 'E' then s else s ^ ".0" in
        Buffer.add_string buf s
    | JString s ->
        Buffer.add_char buf '"';
        String.iter (fun c ->
          match c with
          | '"' -> Buffer.add_string buf "\\\""
          | '\\' -> Buffer.add_string buf "\\\\"
          | '\n' -> Buffer.add_string buf "\\n"
          | '\t' -> Buffer.add_string buf "\\t"
          | '\r' -> Buffer.add_string buf "\\r"
          | '\b' -> Buffer.add_string buf "\\b"
          | '\012' -> Buffer.add_string buf "\\f"
          | c when Char.code c < 32 ->
              Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
          | c -> Buffer.add_char buf c
        ) s;
        Buffer.add_char buf '"'
    | JArray items ->
        Buffer.add_char buf '[';
        let first = ref true in
        List.iter (fun item ->
          if !first then first := false else Buffer.add_string buf ",";
          emit item
        ) items;
        Buffer.add_char buf ']'
    | JObject members ->
        Buffer.add_char buf '{';
        let first = ref true in
        List.iter (fun (k, v) ->
          if !first then first := false else Buffer.add_string buf ",";
          emit (JString k);
          Buffer.add_char buf ':';
          emit v
        ) members;
        Buffer.add_char buf '}'
  in
  emit v;
  Buffer.contents buf

let loc_to_json loc =
  JObject [
    "line", JInt loc.line;
    "col", JInt loc.col;
  ]

let rec typ_to_json = function
  | TInt -> JObject ["kind", JString "int"]
  | TBool -> JObject ["kind", JString "bool"]
  | TFloat32 -> JObject ["kind", JString "float32"]
  | TFloat64 -> JObject ["kind", JString "float64"]
  | TChar -> JObject ["kind", JString "char"]
  | TString -> JObject ["kind", JString "string"]
  | TArray t -> JObject ["kind", JString "array"; "of", typ_to_json t]
  | TStruct (name, fields) ->
      let fields_json = List.map (fun (fn, ft) ->
        JObject ["name", JString fn; "type", typ_to_json ft]
      ) fields in
      JObject ["kind", JString "struct"; "name", JString name; "fields", JArray fields_json]
  | TPtr t -> JObject ["kind", JString "ptr"; "to", typ_to_json t]
  | TBox t -> JObject ["kind", JString "box"; "of", typ_to_json t]
  | TNull -> JObject ["kind", JString "null"]
  | TPtrAny -> JObject ["kind", JString "ptrany"]
  | TInvalid -> JObject ["kind", JString "invalid"]

let binop_to_json = function
  | Add -> JString "add"
  | Sub -> JString "sub"
  | Mul -> JString "mul"
  | Eq -> JString "eq"
  | Neq -> JString "neq"

let param_to_json = function
  | PRef (name, t) ->
      JObject ["kind", JString "ref"; "name", JString name; "type", typ_to_json t]
  | POwn (name, t) ->
      JObject ["kind", JString "own"; "name", JString name; "type", typ_to_json t]

let impl_op_to_json = function
  | ImAdd -> JString "op_add"
  | ImSub -> JString "op_sub"
  | ImMul -> JString "op_mul"
  | ImEq -> JString "op_eq"
  | ImNeq -> JString "op_neq"

let impl_expr_to_json (EImpl (loc, op, func)) =
  JObject [
    "op", impl_op_to_json op;
    "func", JString func;
    "loc", loc_to_json loc;
  ]

let rec expr_to_json = function
  | EInt (loc, v) ->
      JObject ["kind", JString "int"; "value", JInt64 v; "loc", loc_to_json loc]
  | EBool (loc, v) ->
      JObject ["kind", JString "bool"; "value", JBool v; "loc", loc_to_json loc]
  | EFloat (loc, v) ->
      JObject ["kind", JString "float"; "value", JFloat v; "loc", loc_to_json loc]
  | EChar (loc, c) ->
      JObject ["kind", JString "char"; "value", JString (String.make 1 c); "loc", loc_to_json loc]
  | EString (loc, s) ->
      JObject ["kind", JString "string"; "value", JString s; "loc", loc_to_json loc]
  | EVar (loc, name) ->
      JObject ["kind", JString "var"; "name", JString name; "loc", loc_to_json loc]
  | EMove (loc, name) ->
      JObject ["kind", JString "move"; "name", JString name; "loc", loc_to_json loc]
  | EClone (loc, name) ->
      JObject ["kind", JString "clone"; "name", JString name; "loc", loc_to_json loc]
  | EStructLit (loc, name, fields) ->
      let fields_json = List.map (fun (fn, fe) ->
        JObject ["name", JString fn; "value", expr_to_json fe]
      ) fields in
      JObject [
        "kind", JString "structLit";
        "name", JString name;
        "fields", JArray fields_json;
        "loc", loc_to_json loc;
      ]
  | EFieldAccess (loc, e, field) ->
      JObject [
        "kind", JString "fieldAccess";
        "expr", expr_to_json e;
        "field", JString field;
        "loc", loc_to_json loc;
      ]
  | EBinOp (loc, op, e1, e2) ->
      JObject [
        "kind", JString "binOp";
        "op", binop_to_json op;
        "left", expr_to_json e1;
        "right", expr_to_json e2;
        "loc", loc_to_json loc;
      ]
  | EIf (loc, cond, then_expr, else_expr) ->
      let else_json = match else_expr with
        | None -> JNull
        | Some e -> expr_to_json e
      in
      JObject [
        "kind", JString "if";
        "cond", expr_to_json cond;
        "then", expr_to_json then_expr;
        "else", else_json;
        "loc", loc_to_json loc;
      ]
  | EChoose (loc, var, cases, otherwise) ->
      let cases_json = List.map (fun (when_expr, then_expr) ->
        JObject ["when", expr_to_json when_expr; "then", expr_to_json then_expr]
      ) cases in
      let otherwise_json = match otherwise with
        | None -> JNull
        | Some e -> expr_to_json e
      in
      JObject [
        "kind", JString "choose";
        "var", expr_to_json var;
        "cases", JArray cases_json;
        "otherwise", otherwise_json;
        "loc", loc_to_json loc;
      ]
  | ECall (loc, name, args) ->
      JObject [
        "kind", JString "call";
        "name", JString name;
        "args", JArray (List.map expr_to_json args);
        "loc", loc_to_json loc;
      ]
  | EMacro (loc, name, args) ->
      JObject [
        "kind", JString "macro";
        "name", JString name;
        "args", JArray (List.map expr_to_json args);
        "loc", loc_to_json loc;
      ]
  | ECast (loc, e, t) ->
      JObject [
        "kind", JString "cast";
        "expr", expr_to_json e;
        "to", typ_to_json t;
        "loc", loc_to_json loc;
      ]
  | EBlock (loc, stmts, result) ->
      let result_json = match result with
        | None -> JNull
        | Some e -> expr_to_json e
      in
      JObject [
        "kind", JString "block";
        "stmts", JArray (List.map stmt_to_json stmts);
        "result", result_json;
        "loc", loc_to_json loc;
      ]
  | EArrayLit (loc, exprs) ->
      JObject [
        "kind", JString "arrayLit";
        "values", JArray (List.map expr_to_json exprs);
        "loc", loc_to_json loc;
      ]
  | EVoid loc ->
      JObject ["kind", JString "void"; "loc", loc_to_json loc]
  | EAddr (loc, e) ->
      JObject ["kind", JString "addr"; "expr", expr_to_json e; "loc", loc_to_json loc]
  | EDeref (loc, e) ->
      JObject ["kind", JString "deref"; "expr", expr_to_json e; "loc", loc_to_json loc]
  | EWhile (loc, cond, body) ->
      JObject [
        "kind", JString "while";
        "cond", expr_to_json cond;
        "body", expr_to_json body;
        "loc", loc_to_json loc;
      ]
  | ELoop (loc, body) ->
      JObject ["kind", JString "loop"; "body", expr_to_json body; "loc", loc_to_json loc]
  | EFor (loc, var, range, body) ->
      JObject [
        "kind", JString "for";
        "var", JString var;
        "range", expr_to_json range;
        "body", expr_to_json body;
        "loc", loc_to_json loc;
      ]

and stmt_to_json = function
  | SLet (loc, mutable_, name, e) ->
      JObject [
        "kind", JString "let";
        "mutable", JBool mutable_;
        "name", JString name;
        "expr", expr_to_json e;
        "loc", loc_to_json loc;
      ]
  | SAssign (loc, name, e) ->
      JObject [
        "kind", JString "assign";
        "name", JString name;
        "expr", expr_to_json e;
        "loc", loc_to_json loc;
      ]
  | SReturn (loc, e) ->
      JObject ["kind", JString "return"; "expr", expr_to_json e; "loc", loc_to_json loc]
  | SExpr (loc, e) ->
      JObject ["kind", JString "expr"; "expr", expr_to_json e; "loc", loc_to_json loc]
  | SCIntro (loc, content) ->
      JObject ["kind", JString "cIntro"; "content", JString content; "loc", loc_to_json loc]
  | SEmpty loc ->
      JObject ["kind", JString "empty"; "loc", loc_to_json loc]

let def_to_json = function
  | DStruct (loc, name, fields) ->
      let fields_json = List.map (fun (fn, ft) ->
        JObject ["name", JString fn; "type", typ_to_json ft]
      ) fields in
      JObject [
        "kind", JString "struct";
        "name", JString name;
        "fields", JArray fields_json;
        "loc", loc_to_json loc;
      ]
  | DFunc (loc, name, params, ret, body) ->
      let ret_json = match ret with
        | None -> JNull
        | Some t -> typ_to_json t
      in
      JObject [
        "kind", JString "func";
        "name", JString name;
        "params", JArray (List.map param_to_json params);
        "returns", ret_json;
        "body", expr_to_json body;
        "safety", JString "safe";
        "loc", loc_to_json loc;
      ]
  | DFuncUnsafe (loc, name, params, ret, body) ->
      let ret_json = match ret with
        | None -> JNull
        | Some t -> typ_to_json t
      in
      JObject [
        "kind", JString "func";
        "name", JString name;
        "params", JArray (List.map param_to_json params);
        "returns", ret_json;
        "body", expr_to_json body;
        "safety", JString "unsafe";
        "loc", loc_to_json loc;
      ]
  | DFuncTrusted (loc, name, params, ret, body) ->
      let ret_json = match ret with
        | None -> JNull
        | Some t -> typ_to_json t
      in
      JObject [
        "kind", JString "func";
        "name", JString name;
        "params", JArray (List.map param_to_json params);
        "returns", ret_json;
        "body", expr_to_json body;
        "safety", JString "trusted";
        "loc", loc_to_json loc;
      ]
  | DCFuncUnsafe (loc, name, params, ret, code) ->
      let ret_json = match ret with
        | None -> JNull
        | Some t -> typ_to_json t
      in
      JObject [
        "kind", JString "cFunc";
        "name", JString name;
        "params", JArray (List.map param_to_json params);
        "returns", ret_json;
        "code", JString code;
        "safety", JString "unsafe";
        "loc", loc_to_json loc;
      ]
  | DTest (loc, name, body) ->
      JObject [
        "kind", JString "test";
        "name", JString name;
        "body", expr_to_json body;
        "loc", loc_to_json loc;
      ]
  | DModule (loc, name) ->
      JObject ["kind", JString "module"; "name", JString name; "loc", loc_to_json loc]
  | SExport (loc, symbol) ->
      JObject ["kind", JString "export"; "symbol", JString symbol; "loc", loc_to_json loc]
  | SImport (loc, path) ->
      JObject ["kind", JString "import"; "path", JString path; "loc", loc_to_json loc]
  | SImportAs (loc, path, alias) ->
      JObject [
        "kind", JString "importAs";
        "path", JString path;
        "alias", JString alias;
        "loc", loc_to_json loc;
      ]
  | SImportHere (loc, path) ->
      JObject ["kind", JString "importHere"; "path", JString path; "loc", loc_to_json loc]
  | DCMagical (loc, content) ->
      JObject ["kind", JString "cMagical"; "content", JString content; "loc", loc_to_json loc]
  | DCIntro (loc, content) ->
      JObject ["kind", JString "cIntro"; "content", JString content; "loc", loc_to_json loc]
  | DImpl (loc, struct_name, impls) ->
      JObject [
        "kind", JString "impl";
        "struct", JString struct_name;
        "impls", JArray (List.map impl_expr_to_json impls);
        "loc", loc_to_json loc;
      ]

let ast_to_json input_file ast =
  let defs = JArray (List.map def_to_json ast) in
  let root = JObject [
    "defs", defs;
    "files", JArray [JString input_file];
  ] in
  json_string_of_json root ^ "\n"

let save_ast input_file ast output_path =
  let json_str = ast_to_json input_file ast in
  let oc = open_out output_path in
  output_string oc json_str;
  close_out oc
