{
  open Parser
  exception SyntaxError of string

  (* 辅助函数：处理字符串中的转义序列 *)
  let process_escapes s = 
    let len = String.length s in
    let buffer = Buffer.create len in
    let rec loop i = 
      if i >= len then
        Buffer.contents buffer
      else if s.[i] = '\\' && i + 1 < len then
        match s.[i+1] with
        | '\\' -> Buffer.add_char buffer '\\'; loop (i+2)
        | '"' -> Buffer.add_char buffer '"'; loop (i+2)
        | 'n' -> Buffer.add_string buffer "\\n"; loop (i+2)
        | 't' -> Buffer.add_string buffer "\\t"; loop (i+2)
        | 'r' -> Buffer.add_string buffer "\\r"; loop (i+2)
        | 'x' when i + 3 < len -> 
            (* 处理 \xHH 形式的十六进制转义序列 *)
            let hex = String.sub s (i+2) 2 in
            let code = int_of_string ("0x" ^ hex) in
            Buffer.add_char buffer (Char.chr code);
            loop (i+4)
        | '0'..'9' -> 
            (* 处理 \OOO 形式的八进制转义序列 *)
            let rec parse_octal j acc = 
              if j < len && j < i + 4 && s.[j] >= '0' && s.[j] <= '7' then
                parse_octal (j+1) (acc * 8 + (Char.code s.[j] - Char.code '0'))
              else
                (acc, j)
            in
            let (code, next_i) = parse_octal (i+1) 0 in
            Buffer.add_char buffer (Char.chr code);
            loop next_i
        | _ -> 
            (* 其他转义序列，直接添加 *)
            Buffer.add_char buffer s.[i+1];
            loop (i+2)
      else
        (Buffer.add_char buffer s.[i]; loop (i+1))
    in
    loop 0
}

let digit = ['0'-'9']
let digits = digit+
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | [' ' '\t' '\r']     { token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | "/*"                { comment lexbuf; token lexbuf }
  | "//" [^'\n']* '\n'? { Lexing.new_line lexbuf; token lexbuf }
  | '='                 { EQ }
  | ":="                { COLONEQ }
  | "=>"                { DARROW }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | ','                 { COMMA }
  | '.'                 { DOT }
  | ';'                 { raise (SyntaxError ("Semicolon is not allowed in MVP. Statements are terminated by newlines automatically.")) }
  | ':'                 { COLON }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { STAR }
  | "=="                { EQEQ }
  | "!="                { NEQ }
  | "as"                { AS }
  | "struct"            { STRUCT }
  | "ref"               { REF }
  | "move"              { MOVE }
  | "clone"             { CLONE }
  | "print"             { PRINT }
  | "if"                { IF }
  | "elif"              { ELIF }
  | "else"              { ELSE }
  | "mut"               { MUT }
  | "return"            { RETURN }
  | "unsafe"            { UNSAFE }
  | "trusted"           { TRUSTED }
  | "test"              { TEST }
  | "c"                 { C_KEYWORD }
  | "int"               { INT }
  | "bool"              { BOOL }
  | "float32"           { FLOAT32 }
  | "float64"           { FLOAT64 }
  | "char"              { CHAR }
  | "string"            { STRING }
  | "choose"            { CHOOSE }
  | "when"              { WHEN }
  | "otherwise"         { OTHERWISE }
  | "module"            { MODULE }
  | "export"            { EXPORT }
  | "import"            { IMPORT }
  | digits as num       { INT_LIT (Int64.of_string num) }
  | digits '.' digits? as f
                        { FLOAT_LIT (float_of_string f) }
  | '\'' (_ as c) '\''  { CHAR_LIT c }
  | '"' ([^ '"' '\\'] | '\\' _) * '"' as s
                        { let len = String.length s in
                          let content = String.sub s 1 (len - 2) in
                          let processed = process_escapes content in
                          STRING_LIT processed }
  | "\"\"\"" { multi_string "" lexbuf }
  | "true"              { BOOL_LIT true }
  | "false"             { BOOL_LIT false }
  | ident as id         { IDENT id }
  | eof                 { EOF }
  | _ as c              { raise (SyntaxError ("Unexpected char: " ^ String.make 1 c)) }

and multi_string acc = parse
  | "\"\"\"" { STRING_LIT acc }
  | '\n' { Lexing.new_line lexbuf; multi_string (acc ^ "\n") lexbuf }
  | _ as c { multi_string (acc ^ String.make 1 c) lexbuf }

and comment = parse
  | "*/"                { () }
  | eof                 { raise (SyntaxError "Unterminated comment") }
  | _                   { comment lexbuf }