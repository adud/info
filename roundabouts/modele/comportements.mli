open Objets;;

  (*un comportement est une fonction qui prend en entrée des éléments d'une queue d'intersection *)
  
val passif : voiture * int * section * section ->section array -> section array -> unit;;
(*le comportement vide, en quelque sorte*)

val oneone : voiture * int * section * section ->section array -> section array -> unit;;
