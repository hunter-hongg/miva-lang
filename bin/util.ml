open Printf

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

let write_file filename content =
  let channel = open_out filename in  (* 打开文件，如果存在则覆盖 *)
  output_string channel content;
  close_out channel 

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

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

(* ------------------------------------------------------- *)

let clean_line () =
  Printf.eprintf "%s" ("\r" ^ (String.make 100 ' '))