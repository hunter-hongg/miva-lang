[@@@ocaml.warning "-21"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-26"]
open Printf
open Cmdliner
open Util
open Buildrun

module StringSet = Set.Make(String)
module Ast = Mvp_lib.Ast
module Lexer = Mvp_lib.Lexer
module Parser = Mvp_lib.Parser
module Semantic = Mvp_lib.Semantic
module SymbolTable = Mvp_lib.Symbol_table
module Dependency_graph = Mvp_lib.Dependency_graph
module Codegen = Mvp_lib.Codegen
module Global = Mvp_lib.Global
module Macro_expand = Mvp_lib.Macro_expand
module Warning = Mvp_lib.Warnings
module Env = Mvp_lib.Env
module Wrapper = Mvp_lib.Wrapper
open Env
open Wrapper

(* ---------- 全局选项 ---------- *)
let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let release = 
  let doc = "Release mode." in
  Arg.(value & flag & info ["r"; "release"] ~doc)

(* ---------- 子命令: init ---------- *)
let init_name = 
  let doc = "Name of the project to initialize" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let init_type = 
  let doc = "Type of project to initialize (e.g., 'bin', 'lib')" in
  Arg.(required & opt (some string) None & info ["t"; "type"] ~doc)

let init_cmd = 
  let doc = "Initialize a new Miva project" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(
    const (fun _verbose init_name init_type () ->
        Init.init_cmd_do init_name init_type
      )
    $ verbose
    $ init_name
    $ init_type
    $ const ()
  )

(* ---------- 子命令: build/run ---------- *)
let build_new_cmd = 
  let doc = "Compile the Miva project to an executable or a library." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(
    const (fun verbose release () ->
      build_project ~verbose ~release |> ignore
    )
    $ verbose
    $ release
    $ const ()
  )
let run_new_cmd = 
  let doc = "Compile and Run the Miva project." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(
    const (fun verbose release () ->
      let (exe_path, is_lib) = build_project ~verbose ~release in
      if is_lib then (
        eprintf "\x1b[31mError \x1b[0mcannot run a library.\n%!";
        exit 1;
      );
      let run_status = Sys.command exe_path in
      if run_status <> 0 then (
        eprintf "\x1b[31mError \x1b[0mfailed to run the executable.\n";
        exit 1;
      )
    )
    $ verbose
    $ release
    $ const ()
  )
(* ---------- 子命令: clean ---------- *)
let clean_cmd = 
  let doc = "Clean the build artifacts in a Miva project." in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info Term.(
    const (fun _verbose () ->
      Clean.clean_cmd_do ()
    )
    $ verbose
    $ const ()
  )
(* ---------- 子命令: sin-build ---------- *)

let build_input_file =
  let doc = "Miva source file to compile" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let build_output_file =
  let doc = "Output executable name (default: <input> without extension)" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let build_cmd =
  let doc = "Compile an Miva source file to an executable." in
  let info = Cmd.info "sin-build" ~doc in
  Cmd.v info Term.(
    const (fun _verbose input out_opt () ->
      Sinbuild.sin_build_do out_opt input
    )
    $ verbose
    $ build_input_file
    $ build_output_file
    $ const ()
  )

(* ---------- 子命令: sin-run ---------- *)
let run_input_file =
  let doc = "Miva source file to run" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let run_cmd =
  let doc = "Compile and run an Miva program." in
  let info = Cmd.info "sin-run" ~doc in
  Cmd.v info Term.(
    const (fun verbose input () ->
        let output = Filename.basename (Filename.remove_extension input) in
        let exe_path = compile_program ~verbose ~input_file:input 
          ~output_file:output ~project_type:"bin" ~release:false in
        if verbose then eprintf "Running %s...\n%!" exe_path;
        let run_status = Sys.command exe_path in
        (* 清理可执行文件，除非设置了保留标志 *)
        if not (try Sys.getenv "MIVA_KEEP_EXE" <> "" with Not_found -> false) then
          ignore (Sys.command (sprintf "rm -f %s" exe_path));
        exit run_status
      )
    $ verbose
    $ run_input_file
    $ const ()
  )

(* ---------- 子命令: dep ---------- *)

(* ASCII树形结构绘制函数 *)
let print_ascii_tree graph root =
  (* 记录已访问的节点，避免循环 *)
  let visited = ref StringSet.empty in
  
  (* 递归打印树形结构 *)
  let rec print_node node prefix is_last =
    if StringSet.mem node !visited then
      (* 如果节点已被访问，显示为循环引用 *)
      printf "%s%s%s\n%!" prefix (if is_last then "└── " else "├── ") node
    else begin
      visited := StringSet.add node !visited;
      (* 获取当前节点的直接依赖 *)
      let deps = Dependency_graph.get_dependencies graph node in
      let deps_list = StringSet.elements deps in
      let sorted_deps = List.sort String.compare deps_list in
      
      (* 打印当前节点 *)
      printf "%s%s%s\n%!" prefix (if is_last then "└── " else "├── ") node;
      
      (* 递归打印子节点 *)
      let rec print_children children prefix =
        match children with
        | [] -> ()
        | [child] ->
            let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
            print_node child new_prefix true
        | child :: rest ->
            let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
            print_node child new_prefix false;
            print_children rest prefix
      in
      print_children sorted_deps prefix
    end
  in
  
  (* 打印根节点 *)
  printf "%s\n%!" root;
  let deps = Dependency_graph.get_dependencies graph root in
  let deps_list = StringSet.elements deps in
  let sorted_deps = List.sort String.compare deps_list in
  let rec print_root_children children =
    match children with
    | [] -> ()
    | [child] -> print_node child "" true
    | child :: rest ->
        print_node child "" false;
        print_root_children rest
  in
  print_root_children sorted_deps

let dep_cmd =
  let doc = "Show complete dependency graph starting from main.miva" in
  let info = Cmd.info "dep" ~doc in
  Cmd.v info Term.(
    const (fun verbose () ->
      init_env_var ();
      if not (Sys.file_exists "miva.toml") then (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      ) else (
        let toml = read_file "miva.toml" in
        match Toml.Parser.from_string toml with
        | `Ok table -> (
          let project_type =
          try
            match Toml.Types.Table.find (Toml.Min.key "project") table with
            | Toml.Types.TTable t -> (
              match Toml.Types.Table.find (Toml.Min.key "type") t with
              | Toml.Types.TString s -> s
              | _ -> (
                eprintf "Error: 'type' in 'project' table in miva.toml is not a string.\n%!";
                exit 1;
              )
            )
            | _ -> (
              eprintf "Error: 'project' table in miva.toml is not a table.\n%!";
              exit 1;
            )
          with
          | _ -> (
            eprintf "Error: Failed to find 'project' table in miva.toml.\n%!";
            exit 1;
          ) in
          let input_file = (
            if String.compare project_type "lib" == 0 then
              "src/lib.miva"
            else
              "src/main.miva"
          ) in
          if verbose then eprintf "Parsing %s...\n%!" input_file;
          let ast = parse_input_file input_file in
          if verbose then eprintf "Removing duplicate imports...\n%!";
          let ast = remove_duplicate_imports ast in
          if verbose then eprintf "Building symbol table...\n%!";
          let symbol_table = SymbolTable.build_symbol_table ast in
          if verbose then eprintf "Building dependency graph...\n%!";
          let graph = SymbolTable.build_dependency_graph input_file symbol_table in
          printf "\nDependency graph for %s:\n\n%!" input_file;
          print_ascii_tree graph input_file;
          printf "\n";
        )
        | _ -> (
          eprintf "Error: Failed to parse miva.toml.\n%!";
          exit 1;
        )
      )
    )
    $ verbose
    $ const ()
  )

(* ---------- 子命令: test ---------- *)
let test_files =
  let doc = "Specific files to test (optional)" in
  Arg.(value & pos_all string [] & info [] ~doc)

let test_cmd = 
  let doc = "Run all tests in the Miva project or specific files." in
  let info = Cmd.info "test" ~doc in
  Cmd.v info Term.(
    const (fun verbose test_files ->
      if not (find_and_set_project_dir ()) then (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      );
      init_env_var ();
      if not (Sys.file_exists "miva.toml") then (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      ) else (
        let _res = build_project ~verbose in
        let files = list_files (get_cache_dir () ^ "/src") in 
        let all_tests = List.filter (fun f -> String.ends_with ~suffix:".test.cpp" f) files in
        let tests = if test_files = [] then all_tests else List.map (fun f -> (
          f ^ ".test.cpp"
        )) test_files
        in 
        List.iter (fun f -> (
          eprintf "Running %s...\n%!" f;
          let f = (get_cache_dir () ^ "/src/" ^ f) in
          let l = (String.length (get_cache_dir ())) in
          let mvpf = (
            String.sub f (l+1) (String.length f - l - 10)) ^ ".miva" in
          let ast = parse_input_file mvpf in
          let ast = remove_duplicate_imports ast in
          let symb = SymbolTable.build_symbol_table ast in
          let res = List.map (fun s -> (
            let stdp = get_std_include_dir () in
            let objq = if String.starts_with ~prefix:stdp s then (
              (get_cache_dir ()) ^ "/" ^ Filename.remove_extension (remove_prefix stdp s)
            ) else (
              (get_cache_dir ()) ^ "/" ^ (Filename.remove_extension s)
            ) in
            objq ^ ".o"
          )) symb.files in
          let cache_dir = get_cache_dir () in 
          let std_include = get_std_include_dir () in
          let build_dir = get_build_dir () in
          let incf = get_include_flag () in
          let cmd =
            sprintf "g++ -O2 %s -o %s -I%s -I%s -I%s -std=c++20 %s -c %s"
              f ((Filename.remove_extension f) ^ ".o") cache_dir std_include build_dir 
              (if verbose then "" else "2>/dev/null") incf
          in
          let res = res @ [
            (Filename.remove_extension f) ^ ".o"; 
            (Filename.remove_extension (Filename.remove_extension f)) ^ ".o";
          ] in
          if Sys.command cmd <> 0 then (
            eprintf "Error compiling %s\n" f;
            eprintf "Running %s\n%!" cmd;
            exit 1;
          );
          let cmdn = sprintf "g++ %s -o %s %s %s" 
            (String.concat " " res) (Filename.remove_extension f) 
            (if verbose then "" else "2>/dev/null") (get_link_flag ()) in
          if Sys.command cmdn <> 0 then (
            eprintf "Error linking %s\nRunning %s\n%!" f cmdn;
            exit 1;
          );
          let _ = Sys.command (sprintf "./%s" (Filename.remove_extension f)) in
          ()
        )) tests
      )
    )
    $ verbose
    $ test_files
  )

(* ---------- 主命令 ---------- *)
let default_cmd =
  let doc = "The Miva programming language compiler" in
  let man = [
    `S "DESCRIPTION";
    `P "Miva is a systems programming language focused on explicitness, safety, and predictability.";
  ] in
  let info = Cmd.info "miva" ~version:Global.version ~doc ~man in
  Cmd.group info [
    build_cmd; 
    run_cmd; 
    init_cmd; 
    build_new_cmd; 
    run_new_cmd;
    clean_cmd;
    dep_cmd;
    test_cmd;
  ]

let () =
  exit @@ Cmd.eval default_cmd