val p : float (*la probabilite de ralentissement de la voiture*);;

type distr (*une entree, une sortie ou une intersection*);;

type intersection (*une intersection entre deux sections d'autoroute*);;

type voiture (*une voiture*);;

type section (*un fragment de route*);;

val accel : voiture -> int -> unit;;

val desc : voiture -> int -> unit;;

val descrand : voiture -> float -> unit;;

val increment : section -> unit;;

val lier : distr -> distr -> section -> unit;;
