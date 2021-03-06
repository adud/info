val p : float ref(*la probabilite de ralentissement de la voiture*);;

val redm : int ref (*temps de redemarrage apres un arret*);;
  
type distr (*une entree, une sortie ou une intersection*);;

type intersection (*une intersection entre deux sections d'autoroute*);;

type voiture (*une voiture*);;

type section (*un fragment de route*);;

(*creer les objets*)

val dums : section;;

val creer_section : int -> int -> section;;
(*creer_section sz ms cree une section vide de taille sz et de vitesse max ms*)
  
val creer_inter : int -> ( (voiture * int * section * section) option array -> section array -> section list array -> int -> unit) -> distr;;
(*creer_inter n f cree une intersection a n entrees 
dont le comportement est decrit par la fonction de comportement f*)
  
val creer_spawn : unit -> distr;;
(*cree une distribution Spawn*)
  
val creer_sortie : string -> distr;;
(*creer_sortie st cree une sortie de nom st*)
  
val creer_voiture : int -> section list -> string -> voiture;;
(*creer_voiture s d ncree une voiture de vitesse s et d'itineraire d nommee n*)

val itere_voitures : (voiture -> unit) -> section -> unit;;
(*itere_voitures f s : itere f sur l'ensemble des voitures de s*)
  
(*questionner les objets*)

val who : voiture -> string;;

val panneau : section -> int (*un panneau indicateur de vitesse pour la section*);;

val tsec : section -> int (*la taille de la section*);;

val observer : section -> voiture option array (*etudie une section de route*);;

val survoler : section -> bool array;;
(*un tableau representant la section : true s'il y a une voiture*)
  
val radar : voiture -> int (*simule un radar donne la vitesse de la voiture*);;

val firstcar : section -> int ;;
(*regarde ou est la premiere voiture d'une section*)

val patients : distr -> (voiture * int * section * section) option array ;;
(*retourne vitesse et distance de la voiture qui patiente dans l'intersection
en entree (souleve une erreur sinon)*)

val nombre_voitures : section -> int ;;
(*retourne le nombre de voitures presentes dans la section en entree*)

val densite : section list -> float;;
(*retourne la densite de l'ensemble des sections en entree*)

val vitesse_moy : section list -> float;;
(*retourne la vitesse moyenne des voitures de l'ensemble des sections en entree*)

val flot_moy : section list -> float;;
(*retourne le flot moyen de voitures dans l'ensemble des sections en entree*)
  
(*manipuler les objets*)

val ajouter_sortie : distr -> int -> section -> unit;;
(*ajouter sortie d p sec si d est une intersection i : autorise les voitures venant de i.ent.(p) a sortir par sec*)
  
val lier : distr -> int -> distr -> int -> section -> unit;;
(*lier d1 p1 d2 p2 sec fait le lien de la distribution d1 a la distribution d2 par la route sec en positionant sec comme parmi les sorties possibles de d1 et comme entree no p2 de d2*)
  
val ajcar : section -> voiture -> int -> unit;;
(*ajcar s c p ajoute la voiture c dans s a la position p s'il n'y en a pas
deja une*)
val ajcar_sil : section -> voiture -> int -> unit;;
(*une version moins verbeuse de ajcar : ne souleve pas d'echec si
on met une voiture sur une autre, ne fait rien a la place*)

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
