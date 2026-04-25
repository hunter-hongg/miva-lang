open Ast

let version = "0.0.7"
let builtin_functions_typ = [
  ("print", TNull) ;
  ("prints", TNull) ;
  ("println", TNull) ;
  ("printlns", TNull) ;
  ("error", TNull) ;
  ("errors", TNull) ;
  ("errorln", TNull) ;
  ("errorlns", TNull) ;
  ("exit", TNull) ;
  ("abort", TNull) ;
  ("panic", TNull) ;
  ("string_concat", TString) ;
  ("string_parse", TInt) ;
  ("string_length", TInt) ;
  ("string_make", TString) ;
  ("string_from", TString) ;
  ("box_new", TBox(TInvalid)) ;
  ("box_deref", TInvalid) ;
  ("range", TArray(TInt)) ;
  ("ptr_alloc", TPtrAny);
  ("ptr_realloc", TPtrAny);
  ("ptr_free", TNull);
  ("ptr_set", TNull);
]
let builtin_functions = List.map (fun (n, _) -> n) builtin_functions_typ
let deperecated_msg dep inst macro = 
  Printf.sprintf "\"%s\" is deprecated, use %s \"%s\" instead" 
    dep (if macro then "macro" else "function") inst

let notrec_msg dep inst macro = 
  Printf.sprintf "\"%s\" is not recommended, use %s \"%s\" instead" 
    dep (if macro then "macro" else "function") inst

let deperecated_msg_if dep inst macro exmodname modname = 
  if modname = exmodname then None else Some (deperecated_msg dep inst macro)

let notrec_msg_if dep inst macro exmodname modname = 
  if modname = exmodname then None else Some (deperecated_msg dep inst macro)

let deperecated name modname = match name with
  | "prints" -> Some (deperecated_msg "prints" "prints" true)
  | "printlns" -> Some (deperecated_msg "printlns" "printlns" true)
  | "string_concat" -> (deperecated_msg_if "string_concat" "std.str.concat" false "std.str" modname)
  | "string_parse" -> (deperecated_msg_if "string_parse" "std.str.parse_int" false "std.str" modname)
  | "string_length" -> (deperecated_msg_if "string_length" "std.str.len" false "std.str" modname)
  | "string_make" -> (deperecated_msg_if "string_make" "std.str.make" false "std.str" modname)
  | "ptr_alloc" -> (notrec_msg_if "alloc" "std.mem.alloc" false "std.mem" modname)
  | "ptr_realloc" -> (notrec_msg_if "realloc" "std.mem.realloc" false "std.mem" modname)
  | "ptr_free" -> (notrec_msg_if "free" "std.mem.free" false "std.mem" modname)
  | _ -> None
