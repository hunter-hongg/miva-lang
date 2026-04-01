open Ast

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

let rec build_if_chain loc cond then_branch elifs else_opt =
  match elifs with
  | [] -> EIf (loc, cond, then_branch, else_opt)
  | (elif_cond, elif_branch) :: rest ->
      EIf (loc, cond, then_branch, Some (build_if_chain loc elif_cond elif_branch rest else_opt))

let is_uppercase c = 
  if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' then
    true
  else
    false

let is_lowercase c = 
  if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' then
    true
  else
    false

let is_lowercase_or_dot c = 
  if (is_lowercase c) || c = '.' then
    true
  else
    false

let format_loc loc = 
  let line = loc.line in 
  let col = loc.col in
  Printf.sprintf "%d:%d" line col

let check_snake loc name typ = 
  let has_err = ref false in
  let err = ref "" in
  String.iter (fun c -> (
    if (is_uppercase c) && (not !has_err) then (
      has_err := true;
      let warning_msg = Printf.sprintf "Warning:%d:%d:%s%s name '%s'%s" 
        loc.line loc.col "The " typ name " isn't a snake_case name." in
      err := warning_msg
  ))) name;
  if !has_err then
    Some !err
  else 
    None

let check_all_lower loc name typ = 
  let has_err = ref false in
  let err = ref "" in
  String.iter (fun c -> (
    if (not (is_lowercase_or_dot c)) && (not !has_err) then (
      has_err := true;
      let warning_msg = Printf.sprintf "Warning:%d:%d:%s%s name '%s'%s" 
        loc.line loc.col "The " typ name " isn't a lowercase name." in
      err := warning_msg
    )
  )) name;
  if !has_err then
    Some !err
  else 
    None

let write_file filename content =
  let channel = open_out filename in  (* 打开文件，如果存在则覆盖 *)
  output_string channel content;
  close_out channel 

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let get_head lst = 
  match lst with
  | [] -> None
  | h :: _ -> Some h

let string_get_head lst = 
  match get_head lst with
  | Some h -> h
  | None -> ""