let get_home () = 
  try Unix.getenv "HOME" with Not_found -> "/"

let get_build_dir () =
  try Unix.getenv "MIVA_BUILD" with Not_found -> "build/debug"

let get_cache_dir () =
  try Unix.getenv "MIVA_BUILD_CACHE" with Not_found -> "build/debug/cache"

let get_std_include_dir () =
  try Unix.getenv "MIVA_STD" with Not_found -> (
    get_home () ^ "/.miver/lib/"
  )

let get_include_flag () = 
  try Unix.getenv "MIVA_INC_FLAGS" with Not_found -> ""

let get_link_flag () = 
  try Unix.getenv "MIVA_LINK_FLAGS" with Not_found -> ""

let get_cache_dir_rel release = 
  if release then (
    try Unix.getenv "MIVA_RELEASE_CACHE" with Not_found -> "build/release/cache"
  ) else (
    get_cache_dir ()
  )

let get_build_dir_rel release = 
  if release then (
    try Unix.getenv "MIVA_RELEASE_BUILD" with Not_found -> "build/release"
  ) else (
    get_build_dir ()
  )

let get_basic_build_dir () = 
  try Unix.getenv "MIVA_BUILD_BASIC" with Not_found -> "build"