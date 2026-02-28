[@@@ocaml.warning "-21"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-26"]
open Printf
open Cmdliner


module StringSet = Set.Make(String)
module Ast = Mvp_lib.Ast
module Lexer = Mvp_lib.Lexer
module Parser = Mvp_lib.Parser
module Semantic = Mvp_lib.Semantic
module SymbolTable = Mvp_lib.Symbol_table
module Dependency_graph = Mvp_lib.Dependency_graph
module Codegen = Mvp_lib.Codegen
module Global = Mvp_lib.Global

let compute_sha256 filename =
  Digest.to_hex (Digest.file filename)
let list_files dir =
  let dh = Unix.opendir dir in
  let rec read_files acc =
    try
      let entry = Unix.readdir dh in
      if entry = "." || entry = ".." then
        read_files acc
      else
        read_files (entry :: acc)
    with End_of_file ->
      Unix.closedir dh;
      List.rev acc
  in
  read_files []
let write_file filename content =
  let channel = open_out filename in  (* 打开文件，如果存在则覆盖 *)
  output_string channel content;
  close_out channel 

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content
let remove_prefix (a : string) (b : string) : string =
  let len_a = String.length a in
  let len_b = String.length b in
  (* 从索引 len_a 开始，截取 len_b - len_a 个字符 *)
  String.sub b len_a (len_b - len_a)

(* 查找 miva.toml 文件并设置工作目录 *)
let find_and_set_project_dir () : bool =
  let rec search_upwards current_dir =
    let config_path = Filename.concat current_dir "miva.toml" in
    if Sys.file_exists config_path then (
      (* 找到 miva.toml，设置工作目录 *)
      Sys.chdir current_dir;
      true
    ) else (
      let parent_dir = Filename.dirname current_dir in
      (* 如果到达根目录且未找到，返回 false *)
      if parent_dir = current_dir then
        false
      else
        search_upwards parent_dir
    )
  in
  let current_dir = Sys.getcwd () in
  search_upwards current_dir

let init_env_var () = 
  if not (Sys.file_exists "miva.toml") then (
    eprintf "Error: miva.toml not found.\n";
    exit 1 
  );
  let file = read_file "miva.toml" in
  let fail () = 
    eprintf "Error: miva.toml is not a vaild miva project";
    exit 1;
  in
  match Toml.Parser.from_string file with 
  | `Ok table -> (
    match Toml.Types.Table.find (Toml.Min.key "env") table with 
    | Toml.Types.TTable t  -> (
      Toml.Types.Table.iter (fun k v -> (
        match v with
          | Toml.Types.TString s -> (
            let k = (Toml.Types.Table.Key.to_string k) in
            let k = String.uppercase_ascii k in
            Unix.putenv k s
          )
          | _ -> ()
      )) t
    )
    | _ -> ()
  )
  | _ -> (
    fail ()
  )

let clean_line () =
  Printf.eprintf "%s" ("\r" ^ (String.make 100 ' '))

(* ---------- 全局选项 ---------- *)
let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

(* ---------- 公共解析函数 ---------- *)
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

let check_semantics ast =
  try Semantic.check_program ast
  with Failure msg ->
    eprintf "\nSemantic error: %s\n%!" msg;
    exit 1

let check_symbol_table ast =
  try SymbolTable.check_symbols ast
  with Failure msg ->
    eprintf "Symbol table error: %s\n%!" msg;
    exit 1

let check_circular_dependencies file_paths =
  try SymbolTable.check_circular_dependencies file_paths
  with Failure msg ->
    eprintf "\nCircular dependency error: %s\n%!" msg;
    exit 1

let generate_cpp_code inp ast =
  try Codegen.build_ir inp ast
  with Failure msg ->
    eprintf "Codegen error: %s\n%!" msg;
    exit 1


(* ---------- 构建目录管理 ---------- *)
let get_build_dir () =
  try Unix.getenv "MIVA_BUILD" with Not_found -> "build"

let get_cache_dir () =
  try Unix.getenv "MIVA_BUILD_CACHE" with Not_found -> "build/cache"

let get_std_include_dir () =
  try Unix.getenv "MIVA_STD" with Not_found -> "util"

let get_include_flag () = 
  try Unix.getenv "MIVA_INC_FLAGS" with Not_found -> ""

let get_link_flag () = 
  try Unix.getenv "MIVA_LINK_FLAGS" with Not_found -> ""

let ensure_dir_for_file file_path =
  let dir = Filename.dirname file_path in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (sprintf "mkdir -p %s" dir))

let compile_cpp ~verbose ~_input_file ~output_file ~project_type =
  let build_dir = get_build_dir () in
  let cache_dir = get_cache_dir () in
  let std_include = get_std_include_dir () in
  
  let base_name = Filename.remove_extension _input_file in
  let unique_base = base_name in
  
  let cpp_file = unique_base ^ ".cpp" in
  let header_file = unique_base ^ ".h" in
  let obj_file = unique_base ^ ".o" in
  let exe_file = output_file in
  
  (* 确保文件路径的目录存在 *)
  ensure_dir_for_file cpp_file;
  ensure_dir_for_file header_file;
  ensure_dir_for_file exe_file;
  
  let incf = get_include_flag () in
  (* 编译命令 *)
  let cmd =
    sprintf "g++ -O2 %s -o %s -I%s -I%s -I%s -std=c++20 %s %s -c %s"
      cpp_file obj_file cache_dir std_include build_dir 
      (if verbose then "" else "2>/dev/null") 
      (if String.compare project_type "lib" == 0 then "-fPIC" else "") incf
  in
  
  if Sys.command cmd <> 0 then (
    if not verbose then (
      (* 重试并显示详细输出 *)
      let cmd_verbose =
        cmd
      in
      eprintf "\nCompilation failed. Running: %s\n%!" cmd_verbose;
      ignore (Sys.command cmd_verbose)
    );
    eprintf "C++ code saved to %s\n%!" cpp_file;
    exit 1
  );
  
  (* 清理中间文件，除非设置了保留标志 *)
  if not (try Sys.getenv "MIVA_KEEP_CPP" <> "" with Not_found -> false) then (
    ignore (Sys.command (sprintf "rm -f %s" cpp_file));
  );
  
  if verbose then eprintf "\nCompiled successfully: %s\n%!" obj_file;
  obj_file

let link_file ~verbose ~obj_files ~output_file ~project_type =
  let build_dir = get_build_dir () in
  let exe_file = build_dir ^ "/" ^ (
    if String.compare project_type "lib" == 0 then 
      "lib" ^ output_file
    else 
      output_file
  ) in
  let lnkf = get_link_flag () in
  let cmd =
    sprintf "g++ -O2 %s -o %s -std=c++20 %s %s"
      (String.concat " " obj_files) 
      (if String.compare project_type "lib" == 0 then exe_file ^ ".so" else exe_file) lnkf 
      (if String.compare project_type "lib" == 0 then "-fPIC -shared" else "")
  in
  if Sys.command cmd <> 0 then (
    if not verbose then (
      (* 重试并显示详细输出 *)
      let cmd_verbose =
        sprintf "g++ -O2 %s -o %s -std=c++20"
          (String.concat " " obj_files) exe_file
      in
      eprintf "Linking failed. Running: %s\n%!" cmd_verbose;
      ignore (Sys.command cmd_verbose)
    );
    eprintf "Linking failed: %s\n%!" exe_file;
    exit 1
  ) else (
    eprintf "\n%s%!" (String.make 30 '-');
    eprintf "\n\x1b[35mLinking \x1b[0mfiles...\x1b[0m%!"
  );
  if verbose then eprintf "Linked successfully: %s\n%!" exe_file;
  exe_file

(* ---------- 公共编译流程函数 ---------- *)
let compile_program_obj ~verbose ~input_file ~output_file =
  if verbose then eprintf "Parsing %s...\n%!" input_file;
  let ast = parse_input_file input_file in
  if verbose then eprintf "Checking semantics...\n%!";
  check_semantics ast;
  if verbose then eprintf "Checking symbol table...\n%!";
  check_symbol_table ast;
  if verbose then eprintf "Checking circular dependencies...\n%!";
  let symbol_table = SymbolTable.build_symbol_table ast in
  check_circular_dependencies (input_file :: symbol_table.files);
  if verbose then eprintf "Generating C++ code...\n%!";
  let cpp_code = generate_cpp_code input_file ast in
  let cache_dir = get_cache_dir () in
  let base_name = Filename.remove_extension output_file in
  let unique_base = base_name in
  let cpp_file = cache_dir ^ "/" ^ unique_base ^ ".cpp" in
  let cpp_test_file = cache_dir ^ "/" ^ unique_base ^ ".test.cpp" in

  let header_file = cache_dir ^ "/" ^ unique_base ^ ".h" in
  let header_real = unique_base ^ ".h" in
  ensure_dir_for_file cpp_file;
  ensure_dir_for_file header_file;
  let cpp_code_, header_code, test_code = match cpp_code with
    | [cpp; header; test] -> (cpp, header, 
        (if String.length test > 2 then (
          "#include <" ^ header_real ^ ">\n" ^ test
        ) else (""))
      )
    | _ -> failwith "Unexpected codegen output format"
  in

  
  (* 写入C++源代码 *)
  let oc = open_out cpp_file in
  output_string oc cpp_code_;
  close_out oc;

  if String.length test_code > 0 then (
    let oc = open_out cpp_test_file in
    output_string oc test_code;
    close_out oc;
  ) else (
    ()
  );
  
  (* 写入头文件（如果头文件内容不为空） *)
  if header_code <> "" then (
    let oc_header = open_out header_file in
    output_string oc_header header_code;
    close_out oc_header;
    if verbose then eprintf "Generated header file: %s\n%!" header_file;
  );
  cpp_file
let compile_program ~verbose ~input_file ~output_file ~project_type =
  let ast = parse_input_file input_file in
  let symtab = SymbolTable.build_symbol_table ast in
  let graph = SymbolTable.build_dependency_graph input_file symtab in
  let queue = input_file :: symtab.files in
  let hist = [] in
  let need_compile_bin = ref [] in
  (* 实现代码：编译文件*)
  let rec process_queue queue hist obj_paths = 
    match queue with
    | [] -> obj_paths
    | s :: rest ->
        let stdp = get_std_include_dir () in
        let sssn = if String.starts_with ~prefix:stdp s then (
          Filename.remove_extension (remove_prefix stdp s)
        ) else (
          Filename.remove_extension s
        ) in
        let cache_dir = get_cache_dir () in
        let base_name = Filename.remove_extension sssn in
        let unique_base = base_name in
        let md5_file =  cache_dir ^ "/" ^ unique_base ^ ".md5sum" in
        ensure_dir_for_file md5_file;
        let md5sum = compute_sha256 s in
        let snw =  String.concat "/" (List.filter (fun s -> s <> "") (String.split_on_char '/' s)) in
        let snw = (
          if String.starts_with ~prefix:(remove_prefix "/" stdp) snw then (
            (remove_prefix ((remove_prefix "/" stdp) ^ "/") snw)
          ) else (
            snw
          )
        ) in
        let symt = SymbolTable.build_symbol_table (parse_input_file s) in

        let old_sum = if (Sys.file_exists md5_file) then ( 
          read_file md5_file
        ) else "!!!!" in
        let objq = if old_sum = md5sum then (
          cache_dir ^ "/" ^ unique_base ^ ".cpp"
        )
        else (
          write_file md5_file md5sum;
          eprintf "\n\x1b[34mCompiling \x1b[0m%s to cpp file ...\x1b[0m%!" snw;
          let objqn = if String.starts_with ~prefix:stdp s then (
            compile_program_obj ~verbose ~input_file:s ~output_file:(
              Filename.remove_extension (remove_prefix stdp s)
            )
          ) else (
            compile_program_obj ~verbose ~input_file:s ~output_file:(Filename.remove_extension s)
          ) in
          let addfiles = (s :: (StringSet.to_list (Dependency_graph.get_all_dependents graph s))) in
          List.iter ( fun t -> (
            let t = if String.starts_with ~prefix:stdp t then (
              (get_cache_dir ()) ^ "/" 
              ^ (Filename.remove_extension (remove_prefix stdp t)) ^ ".cpp"
            ) else (
              (get_cache_dir ()) ^ "/" 
              ^ (Filename.remove_extension t) ^ ".cpp"
            ) in
            if not (List.mem t !need_compile_bin) then (
              need_compile_bin := t :: !need_compile_bin
            )
          )) addfiles;
          objqn
        ) in

        (* s出队列queue进入历史记录hist，如果queue和hist都没有objp，objp入queue *)
        let new_hist = s :: hist in
        let rest2 = rest in
        let rec process_files files queue hist =
          match files with
          | [] -> queue, hist
          | objp :: rest ->
              let new_queue = if not (List.mem objp queue) && not (List.mem objp hist) then (
                 queue @ [objp] 
              )else (
                queue
              ) in
              process_files rest new_queue hist
        in
        let new_queue, new_hist= process_files symt.files rest2 new_hist in
        process_queue new_queue new_hist (objq :: obj_paths)
  in
  let obj_paths = process_queue queue hist [] in
  eprintf "\n%s%!" (String.make 30 '-');
  let obj_paths_real = List.map (fun s -> (
    if not (List.mem s !need_compile_bin) then (
      let newp = (Filename.remove_extension s) ^ ".o" in
      newp
    ) else (
      let snw = String.concat "/" (List.filter (fun s -> s <> "") (String.split_on_char '/' s)) in
      let snw = remove_prefix (get_cache_dir () ^ "/") snw in
      Printf.eprintf "\n\x1b[34mCompiling \x1b[0m%s to bin...\x1b[0m%!" snw;
      compile_cpp ~verbose ~_input_file:s ~output_file:(Filename.basename s) ~project_type
    )
  )) obj_paths in
  let exe_path = link_file ~verbose ~obj_files:obj_paths_real ~output_file ~project_type in
  exe_path

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
        if Sys.file_exists "miva.toml" then (
          eprintf "Error: Project already initialized in this directory.\n%!";
          exit 1;
        ) else (
          match init_type with
          | "bin" | "lib" -> (
            write_file "miva.toml" (
              sprintf "[project]\nname = \"%s\"\ntype = \"%s\"\n" init_name init_type
            );
            Sys.mkdir "src" 0o755;
            if String.compare init_type "bin" == 0 then (
              write_file "src/main.miva" (
                "// main.miva \n" ^
                "// Generated by Miva init\n" ^
                "module \"main\"\n" ^
                "main = () => {\n" ^
                "  println(\"Hello, World\")\n" ^ 
                "}\n"
              )
            );
            printf "Initialized %s project in %s\n%!" init_type init_name
          )
          | _ -> (
            eprintf "Error: Unknown project type: %s\n%!" init_type;
            exit 1;
          )
        )
      )
    $ verbose
    $ init_name
    $ init_type
    $ const ()
  )

(* ---------- 子命令: build/run ---------- *)
let build_project ~verbose = 
  if not (find_and_set_project_dir ()) then (
    eprintf "Error: Project not initialized in this directory.\n%!";
    exit 1;
  );
  init_env_var ();
  if not (Sys.file_exists "miva.toml") then (
    eprintf "Error: Project not initialized in this directory.\n%!";
    exit 1;
  ) else (
    let toml = read_file "miva.toml" in
    match Toml.Parser.from_string toml with 
    | `Ok table -> (
      let (project_table, project_type) = 
      try 
        match Toml.Types.Table.find (Toml.Min.key "project") table with 
        | Toml.Types.TTable t -> (
          let res = match Toml.Types.Table.find (Toml.Min.key "name") t with 
          | Toml.Types.TString s -> s
          | _ -> (
            eprintf "Error: 'name' in 'project' table in miva.toml is not a string.\n%!";
            exit 1;
          ) in 
          match Toml.Types.Table.find (Toml.Min.key "type") t with 
          | Toml.Types.TString _s -> (
            res, _s
          )
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
        eprintf "Error: Failed to find 'project' table or 'name' in 'project' table in miva.toml.\n%!";
        exit 1;
      ) in
      let input_file = (
        if String.compare project_type "lib" == 0 then 
          "src/lib.miva" 
        else 
          "src/main.miva"
      ) in
      let output_file = project_table in
      let exe_path = compile_program ~verbose ~input_file ~output_file ~project_type in
      printf "\n%s%!" (String.make 30 '-');
      printf ("\n\x1b[32mCompiled successfully: \x1b[0m\n\x1b[36mOutput\x1b[0m %s\n%!") (
        if String.compare project_type "lib" == 0 then 
          exe_path ^ ".so"
        else 
          exe_path
      );
      printf "%s\n%!" (String.make 30 '-');
      if String.compare project_type "lib" == 0 then (
        let copy_file oldfile newfile = 
          let str = read_file oldfile in 
          write_file newfile str
        in
        copy_file ((get_cache_dir ()) ^ "/src/lib.h") ((get_build_dir ()) ^ "/" ^ project_table ^ ".h")
      );
      (exe_path, String.compare project_type "lib" == 0)
    )
    | _ -> (
      eprintf "Error: Failed to parse miva.toml.\n%!";
      exit 1;
    )
  )
let build_new_cmd = 
  let doc = "Compile the Miva project to an executable or a library." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(
    const (fun verbose () ->
      let _res = build_project ~verbose in
      ()
    )
    $ verbose
    $ const ()
  )
let run_new_cmd = 
  let doc = "Compile and Run the Miva project." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(
    const (fun verbose () ->
      let (exe_path, is_lib) = build_project ~verbose in
      if is_lib then (
        eprintf "Error: Cannot run a library.\n%!";
        exit 1;
      );
      let run_status = Sys.command exe_path in
      if run_status <> 0 then (
        eprintf "Error: Failed to run the executable.\n";
        eprintf "Exited with status %d.\n%!" run_status;
        eprintf "You can run it by ./%s\n%!" exe_path;
        exit 1;
      )
    )
    $ verbose
    $ const ()
  )
(* ---------- 子命令: clean ---------- *)
let clean_cmd = 
  let doc = "Clean the build artifacts in a Miva project." in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info Term.(
    const (fun _verbose () ->
      if not (find_and_set_project_dir ()) then (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      );
      init_env_var ();
      if Sys.file_exists "miva.toml" then (
        Sys.command ("rm -rf " ^ (get_build_dir ())) |> ignore;
        printf "Cleaned build artifacts.\n%!"
      ) else (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      )
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
    const (fun verbose input out_opt () ->
        let output = match out_opt with
          | Some s -> s
          | None -> Filename.remove_extension input
        in
        let exe_path = compile_program ~verbose ~input_file:input ~output_file:output ~project_type:"bin" in
        printf "Compiled successfully: %s\n%!" exe_path
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
        let exe_path = compile_program ~verbose ~input_file:input ~output_file:output ~project_type:"bin" in
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