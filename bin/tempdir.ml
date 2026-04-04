let rand_digits () = 
  let rand = Random.State.(bits (make_self_init ()) land 0xFFFFFF) in
  Printf.sprintf "%06x" rand

let mk_temp_dir ?(mode=0o700) ?dir pat = 
  let dir = match dir with 
  | Some d -> d
  | None   -> Filename.get_temp_dir_name ()
  in
  let rec loop count = 
    if count < 0 then failwith "mk_temp_dir: too many failing attempts" else
    let candidate = Filename.concat dir (pat ^ rand_digits ()) in 
    try 
      Unix.mkdir candidate mode; 
      candidate 
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1) (* 重名则重试 *)
    | Unix.Unix_error (Unix.EINTR, _, _)  -> loop count        (* 被中断则重试 *)
    | Unix.Unix_error (e, _, _)           -> 
      failwith ("mk_temp_dir: " ^ Unix.error_message e)
  in
  loop 1000
