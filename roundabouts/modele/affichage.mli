(* les affichages sont suivis d'un print_newline*)
val print_section : Objets.section -> unit (*affiche une section*);;
val draw_section : Objets.section -> unit;;
(*affiche grossierement une section*)
val info_fdmal : Objets.section list -> unit;;
  (*donne les informations du graphe fondamental de la liste de sections :
densite <vitesse> <flux>*)
  (*val print_inter : Objets.distr -> unit*)
  (*affiche les voitures en attente dans la section*)
