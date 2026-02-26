(* 依赖图模块 - 用于管理节点之间的依赖关系 *)
(* 使用双向邻接表实现，支持快速查询依赖和被依赖关系 *)

module StringSet = Set.Make(String)

type t = {
  (* 前向依赖: key -> 依赖key的集合 *)
  dependencies: (string, StringSet.t) Hashtbl.t;
  (* 反向依赖: key -> 被key依赖的集合 *)
  dependents: (string, StringSet.t) Hashtbl.t;
}

(* 创建空的依赖图 *)
let create () = {
  dependencies = Hashtbl.create 16;
  dependents = Hashtbl.create 16;
}

(* 添加依赖关系: node 依赖于 dep *)
let add_dependency t node dep =
  (* 确保两个节点都存在于表中 *)
  if not (Hashtbl.mem t.dependencies node) then
    Hashtbl.add t.dependencies node StringSet.empty;
  if not (Hashtbl.mem t.dependencies dep) then
    Hashtbl.add t.dependencies dep StringSet.empty;
  if not (Hashtbl.mem t.dependents node) then
    Hashtbl.add t.dependents node StringSet.empty;
  if not (Hashtbl.mem t.dependents dep) then
    Hashtbl.add t.dependents dep StringSet.empty;

  (* 更新前向依赖: node 的依赖集合中添加 dep *)
  let deps = Hashtbl.find t.dependencies node in
  Hashtbl.replace t.dependencies node (StringSet.add dep deps);

  (* 更新反向依赖: dep 的被依赖集合中添加 node *)
  let deps = Hashtbl.find t.dependents dep in
  Hashtbl.replace t.dependents dep (StringSet.add node deps)

(* 获取节点的所有直接依赖 *)
let get_dependencies t node =
  try Hashtbl.find t.dependencies node
  with Not_found -> StringSet.empty

(* 获取节点的所有被依赖项 *)
let get_dependents t node =
  try Hashtbl.find t.dependents node
  with Not_found -> StringSet.empty

(* 获取节点的所有间接依赖（递归） *)
let get_all_dependencies t node =
  let rec visit node visited =
    if StringSet.mem node visited then visited
    else
      let visited = StringSet.add node visited in
      let direct_deps = get_dependencies t node in
      StringSet.fold (fun dep acc -> visit dep acc) direct_deps visited
  in
  visit node StringSet.empty |> StringSet.remove node

(* 获取节点的所有间接被依赖项（递归） *)
let get_all_dependents t node =
  let rec visit node visited =
    if StringSet.mem node visited then visited
    else
      let visited = StringSet.add node visited in
      let direct_deps = get_dependents t node in
      StringSet.fold (fun dep acc -> visit dep acc) direct_deps visited
  in
  visit node StringSet.empty |> StringSet.remove node

(* 检查是否存在循环依赖 *)
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

(* 移除依赖关系 *)
let remove_dependency t node dep =
  try
    let deps = Hashtbl.find t.dependencies node in
    let new_deps = StringSet.remove dep deps in
    Hashtbl.replace t.dependencies node new_deps;

    let deps = Hashtbl.find t.dependents dep in
    let new_deps = StringSet.remove node deps in
    Hashtbl.replace t.dependents dep new_deps
  with Not_found -> ()

(* 移除节点及其所有相关依赖 *)
let remove_node t node =
  (* 移除所有依赖该节点的边 *)
  let dependents = get_dependents t node in
  StringSet.iter (fun dep -> remove_dependency t dep node) dependents;

  (* 移除该节点依赖的所有边 *)
  let dependencies = get_dependencies t node in
  StringSet.iter (fun dep -> remove_dependency t node dep) dependencies;

  (* 从表中删除节点 *)
  Hashtbl.remove t.dependencies node;
  Hashtbl.remove t.dependents node

(* 清空依赖图 *)
let clear t =
  Hashtbl.clear t.dependencies;
  Hashtbl.clear t.dependents

(* 获取所有节点 *)
let get_all_nodes t =
  Hashtbl.fold (fun key _ acc -> StringSet.add key acc) t.dependencies StringSet.empty
