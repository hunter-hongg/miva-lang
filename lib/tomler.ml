open Util

let toml_find_table table key = 
  Toml.Types.Table.find (Toml.Min.key key) table

let toml_get_project () = 
  let toml = read_file "miva.toml" in
  try 
    match Toml.Parser.from_string toml with 
    |`Ok table -> (
      match (toml_find_table table "project") with 
      | Toml.Types.TTable t -> (
        Some t
      )
      | _ -> None
    )
    | _ -> None
  with
    | _ -> None

let toml_find_string table key = 
  match (toml_find_table table key) with 
  | Toml.Types.TString s -> Some s
  | _ -> None

let toml_from_import import = 
  if String.starts_with ~prefix:"c:" import then 
    None
  else
    let t = toml_get_project () in
    match t with
    | Some t -> (
      match toml_find_string t "name" with 
        | Some s -> (
          if String.starts_with ~prefix:s import then 
            let res = String.concat "/" ((String.split_on_char '/' import) |> List.tl) in
            let ret = "src/" ^ res ^ ".miva" in
            Some ret
          else 
            let stdpath = try Sys.getenv "MIVA_STD" with _ -> "." in
            let pstack = String.split_on_char '/' import in
            let r = stdpath ^ "/" ^ string_get_head pstack ^ "/src/" ^ (String.concat "/" (List.tl pstack)) ^ ".miva" in
            Some r
        )
        | _ -> None
    )
    | _ -> None