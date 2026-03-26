let red = "\x1b[0;31m"
let green = "\x1b[0;32m"
let yellow = "\x1b[0;33m"
let blue = "\x1b[0;34m"
let magenta = "\x1b[0;35m"
let cyan = "\x1b[0;36m"
let white = "\x1b[0;37m"

let reset = "\x1b[0m"

let colorize color msg =
  color ^ msg ^ reset

let errize msg =
  colorize red ("Error ") ^ msg

let infoize msg = 
  colorize cyan ("Info ") ^ msg