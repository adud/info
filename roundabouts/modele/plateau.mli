open Objets

type plateau;;
(*un plateau de jeu contenant sections et distributions
i.e. un automate*)

type rond_point;;

val rp_ent : rond_point -> section array;;

val rp_sor : rond_point -> section array;;

val rp_ron : rond_point -> section array;;

val rp_dis : rond_point -> distr array;;
(*acceder aux composantes de l'enregistrement*)
  
val creer_rond_point: int -> int -> int -> int -> int -> ( (voiture * int * section * section) option array -> section array -> section list array -> int -> unit)
                      -> rond_point ;;
(*creer_rond_point n l p vmi vme cmp cree un rond-point a n entrees/sorties
de taille l, un anneau interne de taille n*p, des intersection de comportement
de comportement cmp, avec une vitesse max interne(resp externe) de vmi (resp vma)*)

val faire_itin: rond_point -> int -> int -> section list -> section list;;
(*faire itin rp ne ns fin
complete l'itineraire fin en lui ajoutant devant l'itineraire de l'entree ne a la sortie ns*)

val pl_add_rp : plateau -> rond_point -> unit;;
(*pl_add_rp pl rp ajoute le rond-point rp au plateau pl*)
  
val construire: section list -> distr list -> (int -> unit) list -> plateau;;
(*construire s d sp construit le plateau p avec
une liste de sections s
une liste de distributions d
une liste d'evenements (pour l'instant spawn) dependant du temps t*)
  
val iterer : plateau -> int -> unit;;
  (*gere les evenements
   itere l'ensemble des sections 
   puis des intersections du plateau
   si l'horloge vaut t*)

val afficher : plateau -> unit;;
  (*affiche toutes les sections du plateau*)

val imager : plateau -> (section*((int*int)*float*Graphics.color)) list -> unit;;
   
val spawn_car : int -> int -> int -> int -> section -> section list ->int -> unit
(*spawn_car per ph v pos sec itin t 
fait apparaitre une voiture dans sec a la vitesse v position pos 
d'itineraire itin si c'est le moment de faire apparaitre une voiture
i.e. t == ph mod per*)

val rnd_spawn_car : float -> int -> int -> section -> section list ->int -> unit;;
(*rnd_spawn_car p v pos sec itin t version stochastique de spawn_car : fait apparaitre une voiture
avec une probabilite de p *)

val spawn_prog : (int -> float) -> int -> int -> section -> section list -> int -> unit ;;
  (*spawn_prog f ... effectue un rnd_spawn_car avec une proba de (f t) 
permet de faire une apparition croissante des éléments*)

val faire : plateau -> int -> int -> (plateau -> int -> unit) -> (plateau -> int -> unit) -> (plateau -> int -> unit) -> (plateau -> int -> unit) -> unit;;
(*faire p i f dbt bent bsor fin : trivial par induction sur les termes du lambda calcul
*)

val silence : plateau -> int -> int -> unit;;
  (*joue des iterations de l'automate sans rien afficher*)
  
val jouer : plateau -> int -> int -> unit;;
  (*jouer p i f joue (f-i) iterations de l'automate
   en les affichant a chaque tour, l'horloge
commencant a i inclus et finissant a f exclus*)

val animer : plateau -> int -> int -> (section*((int*int)*float*Graphics.color)) list -> unit;;
  (*animer p i f gr anime a l'aide du module Graphics en representant le plateau decrit par gr*)

val modeliser : plateau -> int -> int -> (plateau -> float list) -> unit;;
  (*modeliser p i f info joue (f-i) iterations de l'automate, et affiche les informations collectees par info a chaque tour, espacees d'un retour a la ligne*)

val sauvegarder : plateau -> int -> int -> (plateau -> float list) -> string -> unit;;
  (*idem modeliser, mais au lieu de les afficher, les sauve dans le fichier donne en dernier argument*)
