let version = "0.0.3"
let builtin_functions = [
  "print" ;
  "prints" ;
  "println" ;
  "printlns" ;
  "error" ;
  "errors" ;
  "errorln" ;
  "errorlns" ;
  "exit" ;
  "abort" ;
  "panic" ;
  "string_concat" ;
  "string_parse" ;
  "string_length" ;
  "string_make" ;
  "string_from" ;
  "box_new" ;
  "box_deref" ;
  "range" ;
]
let deperecated_msg dep inst macro = 
  Printf.sprintf "\"%s\" is deprecated, use %s \"%s\" instead" 
    dep (if macro then "macro" else "function") inst

let deperecated_msg_if dep inst macro exmodname modname = 
  if modname = exmodname then None else Some (deperecated_msg dep inst macro)

let deperecated name modname = match name with
  | "prints" -> Some (deperecated_msg "prints" "prints" true)
  | "printlns" -> Some (deperecated_msg "printlns" "printlns" true)
  | "string_concat" -> (deperecated_msg_if "string_concat" "std.str.concat" false "std.str" modname)
  | "string_parse" -> (deperecated_msg_if "string_parse" "std.str.parse_int" false "std.str" modname)
  | "string_length" -> (deperecated_msg_if "string_length" "std.str.len" false "std.str" modname)
  | "string_make" -> (deperecated_msg_if "string_make" "std.str.make" false "std.str" modname)
  | _ -> None
