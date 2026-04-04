open Printf

let sin_build_do out_opt input = 
  let cd = Unix.getcwd () in
  let out = match out_opt with 
  | Some o -> o 
  | None -> (Filename.remove_extension input) ^ ".exe" in
  let tmpdir = Tempdir.mk_temp_dir "mivac-sinbuild-" in
  Unix.chdir tmpdir;
  Init.init_cmd_do "miva-sinbuild" "bin";
  let cur_dir = Unix.getcwd () in
  Unix.chdir cd;
  Util.copy_file input (cur_dir ^ "/src/main.miva");
  Unix.chdir cur_dir;
  let exe, _ = Buildrun.build_project ~verbose:false ~release:false in 
  let exe = cur_dir ^ "/" ^ exe in
  eprintf "Copying %s to %s\n" exe out;
  Unix.chdir cd;
  Util.copy_file exe out

