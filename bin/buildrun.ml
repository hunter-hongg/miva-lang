open Compile
open Printf

open Mvp_lib.Wrapper
open Mvp_lib.Env
open Mvp_lib.Tomler
open Util

module Semantic = Mvp_lib.Semantic
module Macro_expand = Mvp_lib.Macro_expand
module SymbolTable = Mvp_lib.Symbol_table
module Warning = Mvp_lib.Warnings
module Codegen = Mvp_lib.Codegen
module Dependency_graph = Mvp_lib.Dependency_graph

let compile_program ~verbose ~input_file ~output_file ~project_type ~release =  
  eprintf "\n\x1b[34mChecking \x1b[0m%s%!" input_file;
  let _, e = compile_program_obj ~input_file ~output_file:"/tmp/mivc.tmp" ~_release:release ~with_warn:false in
  if e then (
    eprintf "\n";
    exit 1;
  );

  let ast = parse_input_file input_file in
  let ast = Macro_expand.expand_macros ast in
  let ast = remove_duplicate_imports ast in
  let symtab = SymbolTable.build_symbol_table ast in
  let graph = SymbolTable.build_dependency_graph input_file symtab in
  let queue = ((get_std_include_dir ()) ^ "/std/src/str.miva") :: input_file :: symtab.files  in
  let queue = StringSet.elements (StringSet.of_list queue) in

  let hist = [] in
  let need_compile_bin = ref [] in
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
        let cache_dir = get_cache_dir_rel release in
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
        ) else "" in
        let objq = if old_sum = md5sum then (
          cache_dir ^ "/" ^ unique_base ^ ".cpp"
        )
        else (
          write_file md5_file md5sum;
          eprintf "\n\x1b[34mCompiling \x1b[0m%s to cpp file ...\x1b[0m%!" snw;
          try 
            let objqn, err_flag = if String.starts_with ~prefix:stdp s then (
              compile_program_obj ~input_file:s ~output_file:(
                Filename.remove_extension (remove_prefix stdp s)
              ) ~_release:release ~with_warn:true
            ) else (
              compile_program_obj ~input_file:s 
                ~output_file:(Filename.remove_extension s) ~_release:release ~with_warn:true
            ) in
            if err_flag then (
              Sys.remove md5_file;
              eprintf "\n";
              exit 1;
            );
          let addfiles = (s :: (StringSet.to_list (Dependency_graph.get_all_dependents graph s))) in
            List.iter ( fun t -> (
              let t = if String.starts_with ~prefix:stdp t then (
                (get_cache_dir_rel release) ^ "/" 
                ^ (Filename.remove_extension (remove_prefix stdp t)) ^ ".cpp"
              ) else (
                (get_cache_dir_rel release) ^ "/" 
                ^ (Filename.remove_extension t) ^ ".cpp"
              ) in
              if not (List.mem t !need_compile_bin) then (
                need_compile_bin := t :: !need_compile_bin
              )
            )) addfiles;
            objqn
          with Failure msg -> (
            eprintf "\n\x1b[31mError \x1b[0m%s%!" msg;
            Sys.remove md5_file;
            exit 1;
          )
        ) in

        let new_hist = s :: hist in
        let rest2 = rest in
        let rec process_files files queue hist =
          match files with
          | [] -> queue, hist
          | objp :: rest ->
              let new_queue = if not (List.mem objp queue) && not (List.mem objp hist) then (
                 queue @ [objp] 
              ) else (
                queue
              ) in
              process_files rest new_queue hist
        in
        let new_queue, new_hist= process_files symt.files rest2 new_hist in
        process_queue new_queue new_hist (objq :: obj_paths)
  in
  let obj_paths = process_queue queue hist [] in
  let obj_paths_real = List.map (fun s -> (
    if not (List.mem s !need_compile_bin) then (
      let newp = (Filename.remove_extension s) ^ ".o" in
      newp
    ) else (
      let snw = String.concat "/" (List.filter (fun s -> s <> "") (String.split_on_char '/' s)) in
      let snw = remove_prefix ((get_cache_dir_rel release) ^ "/") snw in
      Printf.eprintf "\n\x1b[34mCompiling \x1b[0m%s to bin...\x1b[0m%!" snw;
      try 
        compile_cpp ~verbose ~_input_file:s ~output_file:(Filename.basename s) ~project_type ~release
      with Failure msg -> (
        let md5_file = (Filename.remove_extension s) ^ ".md5sum" in
        eprintf "\n\x1b[31mError: %s\x1b[0m%!" msg;
        Sys.remove md5_file;
        exit 1;
      )
    )
  )) obj_paths in
  let exe_path = link_file ~obj_files:obj_paths_real ~output_file ~project_type in
  exe_path

let parse_fail () = (
  eprintf "\n\x1b[31mError \x1b[0mproject not initialized correctly in this directory.\n%!";
  exit 1;
)

let build_project ~verbose ~release = 
  if not (find_and_set_project_dir ()) then (
    parse_fail ()
  );
  init_env_var ();
  if not (Sys.file_exists "miva.toml") then (
    parse_fail ()
  ) else (
    let t = match toml_get_project () with 
    | Some t -> t
    | None -> parse_fail () in
    let name = match toml_find_string t "name" with 
    | Some s -> s
    | None -> parse_fail () in
    let ptype = match toml_find_string t "type" with 
    | Some t -> t
    | None -> parse_fail () in
    let input_file = (
      if String.compare ptype "lib" == 0 then 
        "src/lib.miva" 
      else 
        "src/main.miva"
    ) in
    let output_file = name in
    let exe_path = compile_program ~verbose ~input_file ~output_file ~project_type:ptype ~release in
    printf ("\n\x1b[32mCompilation suceeded: %s\x1b[0m\n%!") (
      if String.compare ptype "lib" == 0 then 
        exe_path ^ ".so"
      else 
        exe_path
    );
    if String.compare ptype "lib" == 0 then (
    let copy_file oldfile newfile = 
      let str = read_file oldfile in 
      write_file newfile str
      in
      copy_file ((get_cache_dir_rel release) ^ "/src/lib.h") ((get_build_dir_rel release) ^ "/" ^ name ^ ".h")
    );
    (exe_path, String.compare ptype "lib" == 0)
  )