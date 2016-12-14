open Objets;;

  (*un comportement est une fonction qui prend en entrée des éléments d'une queue d'intersection *)
  
val internasch : voiture -> int -> section -> section -> unit;;
(*Applique NaSch a l'intersection*)

val passif : (voiture * int * section * section) option array -> section array -> section array -> unit;;
(*le comportement vide, en quelque sorte*)

val onemany : (voiture * int * section * section) option array -> section array -> section array -> unit;;

(*les comportements faciles : une voiture en entree, une ou plusieurs en sortie*)
