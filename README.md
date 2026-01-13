Flot maximum : Ford-Fulkerson

Ce projet implémente l’algorithme de Ford-Fulkerson pour calculer le flot maximum dans un graphe orienté.

Compilation:
make build


Visualiser le graphe initial (graph1.txt): 
./ftest.exe graphs/graph1.txt 0 0 outfile
dot -Tsvg outfile > graph1_initial.svg


Calcul du flot maximum + visualisation:
make demo graph=graph1.txt src=0 dst=5
dot -Tsvg outfile > graph1_final.svg


graph1_initial.svg : graphe avant l’algorithme

graph1_final.svg : résultat après Ford–Fulkerson