open Printf

module StringSet = Set.Make(String)
module Ast = Mvp_lib.Ast

let compute_sha256 filename =
  Digest.to_hex (Digest.file filename)

(* ------------------------------------------------------- *)

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

let write_file = Mvp_lib.Util.write_file
let read_file = Mvp_lib.Util.read_file

let ensure_dir_for_file file_path =
  let dir = Filename.dirname file_path in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (sprintf "mkdir -p %s" dir))

(* ------------------------------------------------------- *)

let remove_prefix (a : string) (b : string) : string =
  let len_a = String.length a in
  let len_b = String.length b in
  (* 从索引 len_a 开始，截取 len_b - len_a 个字符 *)
  String.sub b len_a (len_b - len_a)

(* ------------------------------------------------------- *)

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

let remove_duplicate_imports defs = 
  let import_set = ref StringSet.empty in
  let filtered_defs = ref [] in
  
  List.iter (fun def ->
    match def with
    | Ast.SImport (_, import) ->
        if not (StringSet.mem import !import_set) then (
          (* eprintf "Hello %s\n" import; *)
          import_set := StringSet.add import !import_set;
          filtered_defs := def :: !filtered_defs
        )
    | Ast.SImportAs (_, import, alias) ->
        let key = import ^ " as " ^ alias in
        if not (StringSet.mem key !import_set) then (
          import_set := StringSet.add key !import_set;
          filtered_defs := def :: !filtered_defs
        )
    | Ast.SImportHere (_, import) ->
        if not (StringSet.mem import !import_set) then (
          import_set := StringSet.add import !import_set;
          filtered_defs := def :: !filtered_defs
        )
    | _ ->
        filtered_defs := def :: !filtered_defs
  ) defs;
  
  List.rev !filtered_defs

(* ------------------------------------------------------- *)

let clean_line () =
  Printf.eprintf "%s" ("\r" ^ (String.make 100 ' '))

let copy_file oldfile newfile = 
  let str = read_file oldfile in 
  write_file newfile str