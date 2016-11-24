val p : float (*la probabilite de ralentissement de la voiture*);;

type distr (*une entree, une sortie ou une intersection*);;

type intersection (*une intersection entre deux sections d'autoroute*);;

type voiture (*une voiture*);;

type section (*un fragment de route*);;

(*creer les objets*)

val creer_section : int -> int -> section;;

val creer_inter : (voiture * int * section * section -> unit) -> distr;;

val creer_spawn : unit -> distr;;

val creer_sortie : string -> distr;;

val creer_voiture : int -> section list -> voiture;;

(*questionner les objets*)

val panneau : section -> int (*un panneau indicateur de vitesse pour la section*);;

val tsec : section -> int (*la taille de la section*);;

val observer : section -> voiture option array (*etudie une section de route*);;

val radar : voiture -> int (*simule un radar donne la vitesse de la voiture*);;

val firstcar : section -> int ;;
(*regarde ou est la premiere voiture d'une section*)

val patients : distr -> (voiture * int * section * section) list ;;
(*affiche vitesse et distance de la voiture qui patiente dans l'intersection
en entree (souleve une erreur sinon)*)

(*manipuler les objets*)

val lier : distr -> distr -> section -> unit;;
(*lier d1 d2 sec fait le lien de la distribution d1 a la distribution
d2 par la route sec*)
val ajcar : section -> voiture -> int -> unit;;
(*ajcar s c p ajoute la voiture c dans s a la position p s'il n'y en a pas
deja une*)


(*iterations de l'automate*)

val accel : voiture -> int -> unit;;

val desc : voiture -> int -> unit;;

val descrand : voiture -> float -> unit;;

val increment : section -> unit;;

val traverser : distr -> unit;;

(*test simple*)

(*val checkpoint : distr;;

val circuit : section;;*)
