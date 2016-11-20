val p : float (*la probabilite de ralentissement de la voiture*);;

type distr (*une entree, une sortie ou une intersection*);;

type intersection (*une intersection entre deux sections d'autoroute*);;

type voiture (*une voiture*);;

type section (*un fragment de route*);;

val creer_section : int -> int -> section;;

val creer_inter : (voiture * int * section * section -> unit) -> distr;;

val creer_spawn : unit -> distr;;

val creer sortie : string -> distr;;

val observer : section -> voiture option array (*etudie une section de route*);;

val radar : voiture -> int (*simule un radar donne la vitesse de la voiture*);;

val accel : voiture -> int -> unit;;

val desc : voiture -> int -> unit;;

val descrand : voiture -> float -> unit;;

val increment : section -> unit;;

val lier : distr -> distr -> section -> unit;;

val traverser : intersection -> unit;;

val checkpoint : distr;;

val circuit : section;;
