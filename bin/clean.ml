open Util
open Color
open Printf
open Env

let clean_cmd_do () =
  if not (find_and_set_project_dir ()) then (
    eprintf "%s\n%!" (errize "project not initialized in this directory");
    exit 1;
  );
  init_env_var ();
  if Sys.file_exists "miva.toml" then (
    Sys.command ("rm -rf " ^ (get_basic_build_dir ())) |> ignore;
    eprintf "%s\n%!" (infoize (sprintf "cleaned build artifacts"));
  ) else (
    eprintf "%s\n%!" (errize "project not initialized in this directory");
    exit 1;
  )
