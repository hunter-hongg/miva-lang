open Printf

let parse_input_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  
  try
    let ast = Parser.program Asi.token lexbuf in
    close_in ic;
    ast
  with
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      let current_token = Lexing.lexeme lexbuf in
      
      (* 读取并显示错误上下文 *)
      let lines = ref [] in
      seek_in ic 0;
      for i = 1 to line + 2 do
        try
          let line_content = input_line ic in
          if i >= max 1 (line - 2) then
            lines := (i, line_content) :: !lines
        with End_of_file -> ()
      done;
      
      eprintf "\n\x1b[31mParser Error\x1b[0m at %s:%d:%d\n" filename line col;
      eprintf "Current token: '%s'\n" current_token;
      eprintf "\nContext:\n";
      
      List.iter (fun (line_num, content) ->
        if line_num = line then
          eprintf "\x1b[31m%5d| %s\x1b[0m\n" line_num content
        else
          eprintf "%5d| %s\n" line_num content
      ) (List.rev !lines);
      
      exit 1
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      eprintf "\n\x1b[31mSyntax Error\x1b[0m at %s:%d:%d\n" filename line col;
      exit 1
  | Lexer.SyntaxError msg ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      eprintf "\n\x1b[31mLexer Error\x1b[0m at %s:%d:%d\n" filename line col;
      eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      eprintf "\n\x1b[31mUnexpected Error\x1b[0m during parsing: %s\n" (Printexc.to_string exn);
      exit 1