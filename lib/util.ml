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