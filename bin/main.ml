[@@@ocaml.warning "-21"]
open Printf

module Ast = Mvp_lib.Ast
module Lexer = Mvp_lib.Lexer
module Parser = Mvp_lib.Parser
module Semantic = Mvp_lib.Semantic
module Codegen = Mvp_lib.Codegen

let usage () =
  eprintf "Usage: %s <input.mvp> [-o <output>]\n%!" Sys.argv.(0);
  exit 1

let stupid () =  usage ()

let () =
  if Array.length Sys.argv < 2 then stupid ();
  flush stderr;

  let input_file = ref "" in
  let output_file = ref "" in
  let i = ref 1 in
  while !i < Array.length Sys.argv do
    (match Sys.argv.(!i) with
    | "-o" ->
        incr i;
        if !i >= Array.length Sys.argv then stupid ();
        output_file := Sys.argv.(!i)
    | f when String.length f > 0 && f.[0] <> '-' ->
        if !input_file <> "" then (
          exit 1
        ) else (
          input_file := f
        )
    | _ ->
        stupid ()
    )
    ;
    incr i
  done;

  if !input_file = "" then stupid ();
  if !output_file = "" then
    output_file := Filename.remove_extension !input_file;

  let ic = open_in !input_file in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf !input_file;

  let ast =
    try
      let prog = Parser.program Lexer.token lexbuf in
      close_in ic;
      prog
    with
    | Parsing.Parse_error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
        eprintf "Syntax error at %s:%d:%d\n%!" !input_file line col;
        exit 1
    | Lexer.SyntaxError msg ->
        eprintf "Lexer error: %s\n%!" msg;
        exit 1
  in

  (try Semantic.check_program ast
   with Failure msg ->
     eprintf "Semantic error: %s\n%!" msg;
     exit 1);

eprintf "Starting codegen...\n%!";

  let cpp_code =
    try Codegen.build_ir ast
    with Failure msg -> (
      eprintf "Codegen error: %s\n%!" msg;
      exit 1;
  ) in 


eprintf "Codegen succeeded!\n%!";

  let cpp_file = !output_file ^ ".cpp" in
  let oc = open_out cpp_file in
  output_string oc cpp_code;
  close_out oc;

  let cmd = sprintf "g++ -O2 %s -o %s 2>/dev/null" cpp_file !output_file in
  if Sys.command cmd <> 0 then (
    let cmd_verbose = sprintf "g++ -O2 %s -o %s" cpp_file !output_file in
    eprintf "Compilation failed. Running: %s\n%!" cmd_verbose;
    let _ = Sys.command cmd_verbose in
    eprintf "C++ code saved to %s\n%!" cpp_file;
    exit 1
  );

  printf "Compiled successfully: %s\n%!" !output_file