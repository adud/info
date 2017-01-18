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

val jouer : plateau -> int -> int -> unit;;
  (*jouer p i f joue (f-i) iterations de l'automate
   en les affichant a chaque tour, l'horloge
commencant a i inclus et finissant a f exclus*)

val animer : plateau -> int -> int -> (Objets.section*((int*int)*float*Graphics.color)) list -> unit;;
  (*animer p i f gr anime a l'aide du module Graphics en representant le plateau decrit par gr*)

val modeliser : plateau -> int -> int -> (Objets.section list -> float) list -> unit;;
  (*modeliser p i f [i1;...;in] joue (f-i) iterations de l'automate, en affichant un fichier csv :
la ligne k correspond a la kieme iteration de l'automate (en partant de i) et la colonne m correspond
a l'information im *)
