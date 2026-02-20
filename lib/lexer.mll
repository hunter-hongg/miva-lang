{
  open Parser
  exception SyntaxError of string
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
  | "else"              { ELSE }
  | "mut"               { MUT }
  | "return"            { RETURN }
  | "unsafe"            { UNSAFE }
  | "trusted"           { TRUSTED }
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
                        { 
                          let s = String.sub s 1 (String.length s - 2) in
                          let buffer = Buffer.create (String.length s) in
                          let rec parse i = 
                            if i >= String.length s then Buffer.contents buffer
                            else if s.[i] = '\\' && i + 1 < String.length s then
                              match s.[i+1] with
                              | 'n' -> Buffer.add_char buffer '\n'; parse (i+2)
                              | 't' -> Buffer.add_char buffer '\t'; parse (i+2)
                              | 'r' -> Buffer.add_char buffer '\r'; parse (i+2)
                              | '"' -> Buffer.add_char buffer '"'; parse (i+2)
                              | '\\' -> Buffer.add_char buffer '\\'; parse (i+2)
                              | c -> Buffer.add_char buffer c; parse (i+2)
                            else
                              (Buffer.add_char buffer s.[i]; parse (i+1))
                          in
                          STRING_LIT (parse 0)
                        }
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