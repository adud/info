type graphe
;;

(*crée un graphe pondere a n sommets sans arete sous 
forme matricielle*) 
val creer_graphe : int -> graphe;;

val taille : graphe -> int;;

(*cree un arc de poids p de a a b dans g*)
val lier : graphe -> int -> int -> float -> unit;;
  
(*supprime l'arc a -> b
ne fait rien si l'arc n'existe pas*)
val suppr : graphe -> int -> int;;

(*succs g s retourne les successeurs du sommet s dans le graphe g*)
val succs : graphe -> int -> int list;;

(*itere_succ f g s itere f sur les successeurs de s dans g*)
val itere_succ : (int -> unit) -> graphe -> int -> unit;;

(*floyd_warshall g applique l'algorithme de Floyd-Warshall au graphe g*)
val floyd_warshall : graphe -> float array array;;

(*acces g a b retourne true ssi il existe un chemin de a a b dans g*)
val acces g a b : graphe -> int -> int -> bool;;

  
