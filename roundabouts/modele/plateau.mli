type plateau;;
(*un plateau de jeu contenant sections et distributions
i.e. un automate*)

val construire: Objets.section list -> Objets.distr list -> (int -> unit) list -> plateau;;
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

val imager : plateau -> (Objets.section*((int*int)*float*Graphics.color)) list -> unit;;
   
val spawn_car : int -> int -> int -> int -> Objets.section -> Objets.section list ->int -> unit
(*spawn_car per ph v pos sec itin t 
fait apparaitre une voiture dans sec a la vitesse v position pos 
d'itineraire itin si c'est le moment de faire apparaitre une voiture
i.e. t == ph mod per*)

val rnd_spawn_car : float -> int -> int -> Objets.section -> Objets.section list ->int -> unit;;
(*rnd_spawn_car p v pos sec itin t version stochastique de spawn_car : fait apparaitre une voiture
avec une probabilite de p *)

val spawn_prog : (int -> float) -> int -> int -> Objets.section -> Objets.section list -> int -> unit ;;
  (*spawn_prog f ... effectue un rnd_spawn_car avec une proba de (f t) 
permet de faire une apparition croissante des éléments*)

val faire : plateau -> int -> int -> (plateau -> unit) -> (plateau -> unit) -> (plateau -> unit) -> (plateau -> unit) -> unit;;
(*faire p i f dbt bent bsor fin : trivial par induction sur les termes du lambda calcul
*)

val silence : plateau -> int -> int -> unit;;
  (*joue des iterations de l'automate sans rien afficher*)
  
val jouer : plateau -> int -> int -> unit;;
  (*jouer p i f joue (f-i) iterations de l'automate
   en les affichant a chaque tour, l'horloge
commencant a i inclus et finissant a f exclus*)

val animer : plateau -> int -> int -> (Objets.section*((int*int)*float*Graphics.color)) list -> unit;;
  (*animer p i f gr anime a l'aide du module Graphics en representant le plateau decrit par gr*)

val modeliser : plateau -> int -> int -> (unit -> unit) -> unit;;
  (*modeliser p i f info joue (f-i) iterations de l'automate, appliquant
info a chaque tour*)
