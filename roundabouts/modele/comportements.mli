open Objets;;

(*un comportement est une fonction gerant la traversee d'une intersection *)
  
val internasch : voiture -> int -> section -> section -> unit;;
(*Applique NaSch a l'intersection*)

val corresp : section -> section -> section array -> section list array -> int -> bool;;
(*corresp e s ent sor i renvoie true si la voiture se trouvant dans l'intersection Inter
a la position  Inter.att.(i) a pu arriver a cette position, et peut en repartir, la ou
elle veut aller*)
  
val passif : (voiture * int * section * section) option array -> section array -> section list array -> int -> unit;;
(*le comportement vide, en quelque sorte*)

val onemany : (voiture * int * section * section) option array -> section array -> section list array -> int -> unit;;

(*les comportements faciles : une voiture en entree, une ou plusieurs en sortie*)

val prioabs : (voiture * int * section * section) option array -> section array -> section list array -> int -> unit;;

(*comportement twomany : deux entrees plusieurs sorties, l'entree 0 a la priorite absolue sur l'entree 0 :
s'il y a une voiture de 0 qui veut passer alors la voiture de 1 ne bouge pas et la laisse passer*)

val priodyn : (voiture * int * section * section) option array -> section array -> section list array -> int -> unit;;
(*priorite 2 entrees du type decrit par Rui-Xiong*)

val feux : int -> int -> int ->  (voiture * int * section * section) option array -> section array -> section list array -> int -> unit;;
(*feux dur1 dur2 ph retourne un comportement de feux : att.(0) passe pendant dur1, puis att.(1) pendant dur2 en commencant par ph*)


(*comportements d'apparition : fonctions gerant la frequence d'apparition des voitures*)

val pente : float -> float -> int -> int -> float;;
  (*pente b e s t retourne pour un automate pendant s etapes, une apparition de voitures croissante en fonction de t allant de b a e*)
