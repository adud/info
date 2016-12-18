val p : float (*la probabilite de ralentissement de la voiture*);;

type distr (*une entree, une sortie ou une intersection*);;

type intersection (*une intersection entre deux sections d'autoroute*);;

type voiture (*une voiture*);;

type section (*un fragment de route*);;

(*creer les objets*)

val dums : section;;

val creer_section : int -> int -> section;;
(*creer_section sz ms cree une section vide de taille sz et de vitesse max ms*)
  
val creer_inter : int -> int -> ( (voiture * int * section * section) option array -> section array -> section array -> int -> unit) -> distr;;
(*creer_inter e s f cree une intersection a e entrees, s sorties, 
dont le comportement est decrit par la fonction *)
  
val creer_spawn : unit -> distr;;
(*cree une distribution Spawn*)
  
val creer_sortie : string -> distr;;
(*creer_sortie st cree une sortie de nom st*)
  
val creer_voiture : int -> section list -> voiture;;
(*creer_voiture s d cree une voiture de vitesse s et d'itineraire d*)

  
(*questionner les objets*)

val panneau : section -> int (*un panneau indicateur de vitesse pour la section*);;

val tsec : section -> int (*la taille de la section*);;

val observer : section -> voiture option array (*etudie une section de route*);;

val radar : voiture -> int (*simule un radar donne la vitesse de la voiture*);;

val firstcar : section -> int ;;
(*regarde ou est la premiere voiture d'une section*)

val patients : distr -> (voiture * int * section * section) option array ;;
(*affiche vitesse et distance de la voiture qui patiente dans l'intersection
en entree (souleve une erreur sinon)*)

(*manipuler les objets*)

val lier : distr -> int -> distr -> int -> section -> unit;;
(*lier d1 p1 d2 p2 sec fait le lien de la distribution d1 a la distribution
d2 par la route sec en positionant sec comme sortie no p1 de d1 et comme entree no p2
de d2*)
  
val ajcar : section -> voiture -> int -> unit;;
(*ajcar s c p ajoute la voiture c dans s a la position p s'il n'y en a pas
deja une*)


(*iterations de l'automate*)

val brake : voiture -> unit;;
(*brake c : fait freiner la voiture*)

val accel : voiture -> int -> unit;;
(*accel c vmax fait accelerer la voiture, dans la limite de vmax*)
    
val desc : voiture -> int -> unit;;
(*desc c dsec fait ralentir la voiture c en-dessous de dsec
eviter les collisions*)

val descrand : voiture -> float -> unit;;
(*descrand c p fait ralentir la voiture c avec une proba p*)
  
val move : int -> section -> unit;;
(*si une voiture se trouve dans la section s a la position k,
fait avancer dans s la voiture s.(k)*)

val ajdir : voiture -> section -> unit;;
(*ajdir v s ajoute la direction s a la voiture v *)

val increment : section -> unit;;
(*increment s itere s selon Nagel-Schreckenberg*)
  
val traverser : distr -> int -> unit;;
  (*traverser d t itere d selon sa fonction de comportement au temps t*)
