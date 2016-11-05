value p : float (*la probabilite de ralentissement de la voiture*);;

type entree (*les entrees de route*);;

type sortie (*les sorties de route*);;

type intersection (*une intersection entre deux sections d'autoroute*);;

type voiture (*une voiture*);;

type section (*un fragment de route*)
;;
  value accel : voiture -> int -> unit
;;
  value desc : voiture -> unit
;;
  value descrand : voiture -> float -> unit
;;
  value increment : section -> unit
;;
  
