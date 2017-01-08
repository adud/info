(* les affichages sont suivis d'un print_newline*)
val scale : float;;
val print_section : Objets.section -> unit (*affiche une section*);;

(*affiche les voitures en attente dans la section*)
val preview_section : Objets.section -> unit;;
(*affiche grossierement une section*)
val info_fdmal : Objets.section list -> unit;;
  (*donne les informations du graphe fondamental de la liste de sections :
densite <vitesse> <flux>*)

(*petite interface graphique des familles*)
val pi : float;;
  
val init : int -> int -> unit;;
(*initialise l'interface*)

val draw_section : Objets.section -> int*int -> float -> Graphics.color -> unit;;
(*dessine une section*)
