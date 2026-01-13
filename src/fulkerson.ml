open Graph
open Tools 

type path = int arc list

let find_path (g: int graph) (origine : id) (dest : id) = 
  (*On vérifie que le graphe possede bien les noeuds origine et dest*)
  if (not (node_exists g origine) || not (node_exists g dest)) then None else 
    (*S'ils existent on fait un DFS en récursivité*)
    let rec find_path_loop origine acu_path visited =
      (*On récupère la liste des arcs sortants depuis le noeud courant*)
      if origine = dest then Some (List.rev acu_path) 
      else 
        let arc_List = out_arcs g origine in 
          let rec explore arcs = 
            match arcs with
            |[]->None
            |current_arc::rest-> let tgt = current_arc.tgt in
                if current_arc.lbl > 0 && not (List.mem tgt visited)  
                  then 
                  match (find_path_loop tgt (current_arc::acu_path) (tgt::visited)) with
                    |Some path -> Some path
                    |None -> explore rest  
                  else explore rest
            in explore arc_List
          in 
            find_path_loop origine [] [origine]


let init_graph_residual (g:int graph) : int graph =
  let gr = clone_nodes g in
  e_fold g (fun acc (e:int arc) ->
       let acc = add_arc acc e.src e.tgt e.lbl in   (* arc avant *)
       let acc = add_arc acc e.tgt e.src 0 in       (* arc inverse,avec capa=0 *)
       acc ) gr

let init_flow_graph (g : int graph) : int graph =
  let fg = clone_nodes g in
  e_fold g (fun acc e -> add_arc acc e.src e.tgt 0) fg

let augmenting_capa (path:int arc list) : int =
  match path with
  | [] -> 0
  | a::rest -> List.fold_left (fun m e -> min m e.lbl) a.lbl rest


let update_graph_residual (gr:int graph) (path:int arc list) (delta:int) : int graph =
  List.fold_left (fun acc e ->
       let acc = add_arc acc e.src e.tgt (-delta) in
       let acc = add_arc acc e.tgt e.src (delta) in acc ) gr path


let update_flow (cap : int graph) (flow : int graph) (path : path) (delta : int) : int graph =
  List.fold_left
    (fun acc e ->
      match find_arc cap e.src e.tgt with
      | Some _ -> add_arc acc e.src e.tgt delta
      | None -> add_arc acc e.tgt e.src (-delta))
    flow path


let ford_fulkerson (g:int graph) (source:id) (puit:id) (fcDFS:( int graph -> id -> id -> path option)) : int * int graph =

  let gr0 = init_graph_residual g in
  let flot0 = init_flow_graph g in

  let rec loop (gr:int graph) (flot : int graph) (flot_tot_acc:int) =
    match (fcDFS gr source puit) with (*recherche de chemin augmentant*)
    | None -> (flot_tot_acc,flot)  
    | Some path ->
        let delta = (augmenting_capa path) in
        if delta <= 0 then (flot_tot_acc,flot)  (* en cas ou un arc est de lbl=0 donc pour eviter boucle infinie *)
        else
          let gr' = update_graph_residual gr path delta in
          let flot' = update_flow g flot path delta in
          loop gr' flot' (flot_tot_acc + delta)
  in
  loop gr0 flot0 0
