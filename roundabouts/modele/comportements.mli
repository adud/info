open Objets;;

  (*un comportement est une fonction qui prend en entrée des éléments d'une queue d'intersection *)
  
val internasch : voiture -> int -> section -> section -> unit;;
(*Applique NaSch a l'intersection*)

val passif : (voiture * int * section * section) option array -> section array -> section array -> int -> unit;;
(*le comportement vide, en quelque sorte*)

val onemany : (voiture * int * section * section) option array -> section array -> section array -> int -> unit;;

(*les comportements faciles : une voiture en entree, une ou plusieurs en sortie*)

val prioabs : (voiture * int * section * section) option array -> section array -> section array -> int -> unit;;

(*comportement twomany : deux entrees plusieurs sorties, l'entree 0 a la priorite absolue sur l'entree 0 :
s'il y a une voiture de 0 qui veut passer alors la voiture de 1 ne bouge pas et la laisse passer*)

val priodyn : (voiture * int * section * section) option array -> section array -> section array -> int -> unit;;
(*priorite 2 entrees du type decrit par Rui-Xiong*)

val feux : int -> int -> int ->  (voiture * int * section * section) option array -> section array -> section array -> int -> unit;;
(*feux dur1 dur2 ph retourne un comportement de feux : att.(0) passe pendant dur1, puis att.(1) pendant dur2 en commencant par ph*)
