open Graph

let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph 
 
let gmap (gr:'a graph) (f:'a->'b) =
  let b_arc (e: 'a arc) = {src = e.src ; tgt = e.tgt ; lbl=f (e.lbl)} in
  let combine (acc: 'b graph) (e: 'a arc) (*-> 'b graph*) = (new_arc acc (b_arc e))
in e_fold gr (combine) (clone_nodes gr)


let add_arc gr id1 id2 n = 
  let arc_created = {src = id1; tgt = id2; lbl = n} in
  let arc_updated (e:'a arc) = {src = id1; tgt =id2 ; lbl=e.lbl+n} in
  let exists_arc = find_arc gr id1 id2 in 
  if (Option.is_none exists_arc) then (new_arc gr arc_created) else (new_arc gr (arc_updated (Option.get exists_arc))) 



