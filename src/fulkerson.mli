open Graph

type path = int arc list

val find_path: g: int graph ->origine : id -> dest : id -> path option

val ford_fulkerson: g :int graph -> source : id ->dest : id -> (g: int graph -> origine : id -> dest : id  -> path option) ->int 


 

