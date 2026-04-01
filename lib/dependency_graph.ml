module StringSet = Set.Make(String)

type t = {
  dependencies: (string, StringSet.t) Hashtbl.t;
  dependents: (string, StringSet.t) Hashtbl.t;
}

let create () = {
  dependencies = Hashtbl.create 16;
  dependents = Hashtbl.create 16;
}

let add_dependency t node dep =
  if not (Hashtbl.mem t.dependencies node) then
    Hashtbl.add t.dependencies node StringSet.empty;
  if not (Hashtbl.mem t.dependencies dep) then
    Hashtbl.add t.dependencies dep StringSet.empty;
  if not (Hashtbl.mem t.dependents node) then
    Hashtbl.add t.dependents node StringSet.empty;
  if not (Hashtbl.mem t.dependents dep) then
    Hashtbl.add t.dependents dep StringSet.empty;

  let deps = Hashtbl.find t.dependencies node in
  Hashtbl.replace t.dependencies node (StringSet.add dep deps);

  let deps = Hashtbl.find t.dependents dep in
  Hashtbl.replace t.dependents dep (StringSet.add node deps)

let get_dependencies t node =
  try Hashtbl.find t.dependencies node
  with Not_found -> StringSet.empty

let get_dependents t node =
  try Hashtbl.find t.dependents node
  with Not_found -> StringSet.empty

let get_all_dependencies t node =
  let rec visit node visited =
    if StringSet.mem node visited then visited
    else
      let visited = StringSet.add node visited in
      let direct_deps = get_dependencies t node in
      StringSet.fold (fun dep acc -> visit dep acc) direct_deps visited
  in
  visit node StringSet.empty |> StringSet.remove node

let get_all_dependents t node =
  let rec visit node visited =
    if StringSet.mem node visited then visited
    else
      let visited = StringSet.add node visited in
      let direct_deps = get_dependents t node in
      StringSet.fold (fun dep acc -> visit dep acc) direct_deps visited
  in
  visit node StringSet.empty |> StringSet.remove node

let has_cycle t =
  let rec dfs visiting visited node =
    if StringSet.mem node visiting then true
    else if StringSet.mem node visited then false
    else
      let visiting = StringSet.add node visiting in
      let visited = StringSet.add node visited in
      let deps = get_dependencies t node in
      StringSet.exists (dfs visiting visited) deps
  in
  Hashtbl.fold (fun key _ acc -> acc || dfs StringSet.empty StringSet.empty key)
    t.dependencies false

let remove_dependency t node dep =
  try
    let deps = Hashtbl.find t.dependencies node in
    let new_deps = StringSet.remove dep deps in
    Hashtbl.replace t.dependencies node new_deps;

    let deps = Hashtbl.find t.dependents dep in
    let new_deps = StringSet.remove node deps in
    Hashtbl.replace t.dependents dep new_deps
  with Not_found -> ()

let remove_node t node =
  let dependents = get_dependents t node in
  StringSet.iter (fun dep -> remove_dependency t dep node) dependents;

  let dependencies = get_dependencies t node in
  StringSet.iter (fun dep -> remove_dependency t node dep) dependencies;

  Hashtbl.remove t.dependencies node;
  Hashtbl.remove t.dependents node

let clear t =
  Hashtbl.clear t.dependencies;
  Hashtbl.clear t.dependents

let get_all_nodes t =
  Hashtbl.fold (fun key _ acc -> StringSet.add key acc) t.dependencies StringSet.empty
