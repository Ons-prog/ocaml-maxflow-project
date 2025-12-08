open Graph


let find_path (g: int graph) (origine : id) (dest : id) = 
  (*On vérifie que le graphe possede bien les noeuds origine et dest*)
  if (not (node_exists g origine) || not (node_exists g dest)) then None else 
    (*S'ils existent on fait un DFS en récursivité*)
    let rec find_path_loop origine acu_path visited =
      (*On récupère la liste des arcs sortants depuis le noeud courant*)
      if origine == dest then Some (List.rev acu_path) 
      else 
        let arc_List = out_arcs g origine in 
          let rec explore arcs = 
            match arcs with
            |[]->None
            |current_arc::rest-> let tgt = current_arc.tgt in
                if (not (List.mem tgt visited))  
                  then 
                  match (find_path_loop tgt (current_arc::acu_path) (tgt::visited)) with
                    |Some path -> Some path
                    |None -> explore rest  
                  else explore rest
            in explore arc_List
          in 
            find_path_loop origine [] [origine]



