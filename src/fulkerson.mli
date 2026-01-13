open Graph

type path = int arc list

val find_path : int graph -> id -> id -> path option

val ford_fulkerson :
  int graph -> id -> id ->
  (int graph -> id -> id -> path option) ->
  int * int graph

 
