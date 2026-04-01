open Printf

let parse_input_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      eprintf "Syntax error at %s:%d:%d\n%!" filename line col;
      exit 1
  | Lexer.SyntaxError msg ->
      eprintf "Lexer error: %s\n%!" msg;
      exit 1
  | _ ->
      eprintf "Parsing error\n%!";
      exit 1
