(* Le but de ce TP est de résoudre le jeu de Taquin dans sa version
 4*4. Voir https://fr.wikipedia.org/wiki/Taquin pour une présentation
 de ce jeu.
*)

(* Le type utilisé pour représenter les taquins.
   ATTENTION : il est fortement déconseillé de construire soi-même un
   taquin et encore plus de modifier la matrice contenue dans le
   taquin.
   Si vous avez besoin de construire des taquins, utilisez les autres
   fonctions proposées ci-dessous.
 *)
(* Un taquin est représenté par une matrice grille telle que
   grille.(i).(j) vaut le numéro donnant 0 si la case située ligne i
   colonne j est vide, et le numéro du carreau qu'elle contient
   sinon.
   lig et col contiennent respectivement le numéro de ligne et de
   colonne de la case vide.
   Les numéros de ligne et de colonne commencent à zéro.
 *)
type taquin = { grille : int vect vect; lig:int; col:int };;

(* Dans tout ce qui suit, les grilles de taquin seront toutes de
   dimension nblig * nbcol :
 *)
let nblig = 4;;
let nbcol = 4;;

(**** Fonctions auxilaires, dont vous n'aurez normalement pas besoin ****)
(* copie_matrice : 'a vect vect -> 'a vect vect
   Réalise une copie de la matrice passée en argument.
 *)
let copie_matrice a =
  let n = vect_length a in
  let b = make_vect n [| |] in
  for i = 0 to n-1 do
    b.(i) <- copy_vect a.(i)
  done;
  b
;;

(* copie_taquin : taquin -> taquin
   Réalise une copie du taquin qui lui est passé en argument *)
(* NB : remarquez l'usage d'un motif irréfutable dans la définition ci-dessous *)
let copie_taquin { grille = g; lig = i; col = j; } =
  { grille = copie_matrice g; lig = i; col = j }
;;
(**** Fin des fonctions auxiliaires ****)


(* affiche_taquin : taquin -> unit 
   Affiche le taquin passé en argument.
 *)
let affiche_taquin { grille = g; lig = it; col = jt } =
  for i = 0 to nblig-1 do
    for j = 0 to nbcol-1 do
      if (it, jt) = (i, j) then
	print_string "   "
      else
	printf__printf "%02d " g.(i).(j);
    done;
    print_newline();
  done
;;

(* voisins : taquin -> taquin list
   (voisins t) calcule les taquins voisins du taquin donné en
   argument, c'est-à-dire ceux qu'on peut atteindre à partir de t en
   un mouvement.
 *)			 
let voisins { grille = g; lig = it; col = jt; } =
  let res = ref [] in
  (* (mouvement i j) ajoute à res la grille obtenue en déplaçant le trou en
   it+i, jt+j si cela est possible.
   i ou j doit valoir +1 ou -1 et l'autre doit valoir 0 *)
  let mouvement i j =
    let l = it+i in
    let c = jt+j in
    if 0 <= l && l < nblig && 0 <= c && c < nbcol then
      begin
	let g' = copie_matrice g in
	g'.(it).(jt) <- g'.(l).(c);
	g'.(l).(c) <- 0;
	res := { grille = g'; lig = l; col = c; } :: !res
      end
  in
  mouvement (-1)  0;
  mouvement   1   0;
  mouvement   0 (-1);
  mouvement   0   1;
  !res
;;

(* taquin_solution : taquin.
   La solution du taquin.
*)
let taquin_solution =
  let g = make_matrix nblig nbcol 0 in
  for i = 0 to nblig-1 do
    for j = 0 to nbcol-1 do
      g.(i).(j) <- nbcol * i + j + 1;
    done
  done;
  g.(nblig-1).(nbcol-1) <- 0;
  { grille = g; lig = nblig-1; col = nbcol-1; }
;;

(**** Exemples de taquins à résoudre. ****)
(* On commencera par les plus simples.
   Dans ce tableau, exemple.(i) est un taquin situé à exactement i
   mouvements de la solution.
   Le tableau exemple contient 40 exemples de taquin, le dernier est donc à 39
   mouvements de la solution.
 *)

let exemple = [|
    {grille =
    [|[|1; 2; 3; 4|]; [|5; 6; 7; 8|]; [|9; 10; 11; 12|]; [|13; 14; 15; 0|]|];
   lig = 3; col = 3};
  {grille =
    [|[|1; 2; 3; 4|]; [|5; 6; 7; 8|]; [|9; 10; 11; 0|]; [|13; 14; 15; 12|]|];
   lig = 2; col = 3};
  {grille =
    [|[|1; 2; 3; 4|]; [|5; 6; 7; 0|]; [|9; 10; 11; 8|]; [|13; 14; 15; 12|]|];
   lig = 1; col = 3};
  {grille =
    [|[|1; 2; 3; 4|]; [|5; 6; 0; 7|]; [|9; 10; 11; 8|]; [|13; 14; 15; 12|]|];
   lig = 1; col = 2};
  {grille =
    [|[|1; 2; 0; 4|]; [|5; 6; 3; 7|]; [|9; 10; 11; 8|]; [|13; 14; 15; 12|]|];
   lig = 0; col = 2};
  {grille =
    [|[|1; 2; 4; 0|]; [|5; 6; 3; 7|]; [|9; 10; 11; 8|]; [|13; 14; 15; 12|]|];
   lig = 0; col = 3};
  {grille =
    [|[|1; 2; 4; 7|]; [|5; 6; 3; 0|]; [|9; 10; 11; 8|]; [|13; 14; 15; 12|]|];
   lig = 1; col = 3};
  {grille =
    [|[|1; 2; 4; 7|]; [|5; 6; 3; 8|]; [|9; 10; 11; 0|]; [|13; 14; 15; 12|]|];
   lig = 2; col = 3};
  {grille =
    [|[|1; 2; 4; 7|]; [|5; 6; 3; 8|]; [|9; 10; 0; 11|]; [|13; 14; 15; 12|]|];
   lig = 2; col = 2};
  {grille =
    [|[|1; 2; 4; 7|]; [|5; 6; 3; 8|]; [|9; 0; 10; 11|]; [|13; 14; 15; 12|]|];
   lig = 2; col = 1};
  {grille =
    [|[|1; 2; 4; 7|]; [|5; 0; 3; 8|]; [|9; 6; 10; 11|]; [|13; 14; 15; 12|]|];
   lig = 1; col = 1};
  {grille =
    [|[|1; 2; 4; 7|]; [|0; 5; 3; 8|]; [|9; 6; 10; 11|]; [|13; 14; 15; 12|]|];
   lig = 1; col = 0};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|0; 6; 10; 11|]; [|13; 14; 15; 12|]|];
   lig = 2; col = 0};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|13; 6; 10; 11|]; [|0; 14; 15; 12|]|];
   lig = 3; col = 0};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|13; 6; 10; 11|]; [|14; 0; 15; 12|]|];
   lig = 3; col = 1};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|13; 0; 10; 11|]; [|14; 6; 15; 12|]|];
   lig = 2; col = 1};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|0; 13; 10; 11|]; [|14; 6; 15; 12|]|];
   lig = 2; col = 0};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|14; 13; 10; 11|]; [|0; 6; 15; 12|]|];
   lig = 3; col = 0};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|14; 13; 10; 11|]; [|6; 0; 15; 12|]|];
   lig = 3; col = 1};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|14; 13; 10; 11|]; [|6; 15; 0; 12|]|];
   lig = 3; col = 2};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 3; 8|]; [|14; 13; 0; 11|]; [|6; 15; 10; 12|]|];
   lig = 2; col = 2};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 0; 8|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 1; col = 2};
  {grille =
    [|[|1; 2; 4; 7|]; [|9; 5; 8; 0|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 1; col = 3};
  {grille =
    [|[|1; 2; 4; 0|]; [|9; 5; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 0; col = 3};
  {grille =
    [|[|1; 2; 0; 4|]; [|9; 5; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 0; col = 2};
  {grille =
    [|[|1; 0; 2; 4|]; [|9; 5; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 0; col = 1};
  {grille =
    [|[|0; 1; 2; 4|]; [|9; 5; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 0; col = 0};
  {grille =
    [|[|9; 1; 2; 4|]; [|0; 5; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 1; col = 0};
  {grille =
    [|[|9; 1; 2; 4|]; [|5; 0; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 1; col = 1};
  {grille =
    [|[|9; 0; 2; 4|]; [|5; 1; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 0; col = 1};
  {grille =
    [|[|9; 2; 0; 4|]; [|5; 1; 8; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 0; col = 2};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 0; 7|]; [|14; 13; 3; 11|]; [|6; 15; 10; 12|]|];
   lig = 1; col = 2};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|14; 13; 0; 11|]; [|6; 15; 10; 12|]|];
   lig = 2; col = 2};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|14; 13; 11; 0|]; [|6; 15; 10; 12|]|];
   lig = 2; col = 3};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|14; 13; 11; 12|]; [|6; 15; 10; 0|]|];
   lig = 3; col = 3};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|14; 13; 11; 12|]; [|6; 15; 0; 10|]|];
   lig = 3; col = 2};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|14; 13; 0; 12|]; [|6; 15; 11; 10|]|];
   lig = 2; col = 2};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|14; 0; 13; 12|]; [|6; 15; 11; 10|]|];
   lig = 2; col = 1};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|0; 14; 13; 12|]; [|6; 15; 11; 10|]|];
   lig = 2; col = 0};
  {grille =
    [|[|9; 2; 8; 4|]; [|5; 1; 3; 7|]; [|6; 14; 13; 12|]; [|0; 15; 11; 10|]|];
   lig = 3; col = 0}
   |]
;;
  
(* J'ai constaté sur le taquin suivant que le nombre de mouvements
   nécessaire est au moins égal à 56. J'ai trouvé une solution en 180
   mouvements, ce qui n'est pas optimal (on peut montrer que 80
   mouvements suffisent pour toute configuration).
 *)
let taquin_difficile = {
  grille =
    [|[|7; 12; 5; 11|]; [|8; 3; 1; 14|]; [|15; 9; 13; 2|]; [|4; 0; 6; 10|]|];
  lig = 3; col = 1 }
;;

(* Le nombre de mouvements du taquin suivant est égal à 80 d'après la
   thèse de Ralph Udo Gasser, Harnessing Computational Resources,
   1995, chap 6, section 5.
   Disponible sur
   https://pdfs.semanticscholar.org/25dd/f98704a27aa451dc5c7f75eedc07f5f3f614.pdf
   J'arrive à trouver une solution en 160 mouvements et à constater
   que le nombre de mouvements nécessaire est au moins égal à 70.
*)
let taquin_le_plus_difficile = {
  grille = [|[|0;12;10;13|];[|15;11;14;9|];[|7;8;6;2|];[|4;3;5;1|]|];
  lig = 0; col = 0 }
;;

(************* Ensembles impératifs *****************)
(* On implante un type 'a ensemble permettant de stocker, de façon
   impérative, des éléments de type 'a.
 *)
type 'a ensemble == ('a, unit) hashtbl__t
;;

(* l'ensemble vide *)
let (ensemble_vide : unit -> 'a ensemble) =
  fun () -> hashtbl__new 1973;;

(* (ajoute e x) ajoute l'élément x à l'ensemble e. *)
let (ajoute : 'a ensemble -> 'a -> unit) =
  fun e x -> hashtbl__add e x ();;

(* 
   (appartient x e) retourne un booléen disant si x appartient à e
 *)
let (appartient : 'a -> 'a ensemble -> bool) =
  fun x e -> hashtbl__find_all e x <> [];;

(************* Fin ensembles impératifs ***********)

(************* Fonctions de recherche à implanter **********)

(* On veut étant donné un taquin t, trouver un chemin vers la solution
   taquin_solution.

   On dira qu'une liste [t0; ...; t(n-1)] est un chemin de t vers la
   solution si t0 = t que t0, ..., t(n-1), taquin_solution est une
   suite de taquins tels qu'on peut passer de t(i) à t(i+1) en un
   mouvement (pour i=0, ..., i=n-2).

   En particulier si t = taquin_solution, la liste vide est un chemin
   vers la solution.
 *)
(* rech_largeur : taquin -> taquin list
   (rech_largeur t) effectue une recherche en largeur et retourne un
   chemin vers la solution taquin_solution (on suppose qu'il y en a une).
 *)
  
(* À VOUS DE JOUER. Vous pourrez notamment utiliser le module
   prédéfini queue pour gérer des files.
 *)

let rech_largeur s_initial =
;;


(* Sur mon PC, la recherche en largeur marche pour taquin13 (même si
   la réponse n'est pas immédiate. En revanche, pour taquin16, elle
   consomme trop de mémoire et échoue avec l'erreur Out_of_memory.

   Il est donc intéressant d'essayer d'autres stratégies de recherche.

   La recherche en profondeur telle que vue dans le cours consommerait
   autant de mémoire voire plus. De plus, elle ne retournerait
   probablement pas les chemins les plus courts.

   On va essayer un autre type de recherche en profondeur : on cherche
   en profondeur jusqu'à une profondeur bornée, sans mémoriser les
   configurations par lesquelles on est passé.

   Plus précisément, on écrira une fonction
   rech_chemin_borne :
   taquin -> int -> int -> taquin list -> taquin list option
   L'appel (rech_chemin_borne t b n [s(n-1); ...; s(0)]) doit permettre
   de dire s'il existe un chemin commençant par
   [s(0); ...; s(n-1); s ], de longueur au plus b et arrivant à la solution :

   * si un tel chemin c existe, il est retourné sous la forme (Some c)

   * sinon, on retourne None.

   L'implantation peut se faire récursivement de la façon suivante :

   * si n > b, on retourne None

   * sinon, si t = taquin_solution, on retourne Some [s(0); ...; s(n-1)]

   * sinon, on essaie, pour chaque t' voisin de t, d'exécuter
     (rech_chemin_borne t' b (n+1) [t; s(0); ...; s(n-1)]) ; si pour au moins l'un de
     ces voisins, le résultat est (Some c), on retourne (Some
     c). Sinon, on retourne None.
 *)

(* À VOUS DE JOUER *)
let rec rech_chemin_borne t b n c =
;;

(* On peut vérifier que rech_chemin_borne permet par exemple de
  résoudre jusqu'à taquin12 (au prix d'un peu de patience cependant).

  Cette méthode comporte deux défauts majeurs :

  * On ne sait pas a priori à quelle profondeur chercher.

  * Si on cherche à une profondeur trop élevée, on ne trouve pas
    toujours la meilleure solution (essayer sur des exemples).

  *)	   

(* rech_par_approfondissement : taquin -> taquin list
   Cette recherche consiste à simplement à essayer rech_chemin_borne
   pour des valeurs successives de la borne sur la longueur du chemin
   égale successivement à 0, 1, 2, ... jusqu'à trouver une solution.
 *)
(* À VOUS DE JOUER *)
let rec rech_par_approfondissement t =
;;

(* Pour améliorer cette recherche, on pourrait simplement éliminer de
   notre recherche les chemins non-simples : on va donc écrire une fonction
   rech_chemin_simple_borne :
   taquin -> int -> int -> taquin list -> taquin list option
   L'appel (rech_chemin_simple_borne t b n [s(n-1); ...; s(0)]) doit permettre
   de dire s'il existe un chemin simple commençant par
   [s(0); ...; s(n-1); s ], de longueur au plus b et arrivant à la
   solution.

   On implantera cette fonction comme précédemment, à une différence
   près, le test initial : Au lieu de retourner None lorsque n > b, on
   retournera None lorsque n > b OU t appartient à [s(n-1); ...;
   s(0)] (on pourra utiliser la fonction mem).
 *)
(* À VOUS DE JOUER *)
let rec rech_chemin_simple_borne t b n c =
;;


(* Essayez votre fonction sur taquin12, taquin13 et taquin16 *)

(* Comme précédemment, on peut faire une recherche par
   approfondissement.

   rech_par_approfondissement2 : taquin -> taquin list   

   Effectue une recherche par approfondissements successifs en
   utilisant rech_chemin_simple_borne.
 *)
(* À VOUS DE JOUER *)
let rec rech_par_approfondissement2 t =
;;

(* Remarquez que rech_par_approfondissement2 donne les mêmes solutions
   que rech_par_approfondissement mais les donne plus vite.

   rech_chemin_simple_borne utilise en effet le fait que les chemins
   les plus courts sont nécessairement simples pour échouer plus vite
   dans ses recherches.

   Échouer rapidement, ou plus exactement, se rendre compte rapidement
   compte qu'on se fourvoie, est de manière générale un ingrédient
   essentiel des algorithmes de recherche.

   On peut donc chercher à échouer plus rapidement dans les requêtes
   précédentes. Une façon de faire est d'utiliser une fonction
   h : taquin -> int qui donnera une minoration du nombre de
   mouvements qu'il reste à parcourir pour trouver la solution.

   On donne ci-dessous deux fonctions h1 et h2 qui minorent ce nombre
 de mouvements.
 *)

(* h1 : taquin -> int
   Retourne le nombre de carreaux mal placés
 *)
let h1 t =
  let g1 = t.grille in
  let g2 = taquin_solution.grille in
  let c = ref 0 in
  for i = 0 to nblig-1 do
    for j = 0 to nbcol-1 do
      if g1.(i).(j) <> g2.(i).(j) && g1.(i).(j) <> 0 then
	incr c
    done
  done;
  !c
;;

let pos v =
  (* position normale du carreau de valeur v dans la grille *)
  let vm1 = v - 1 in
  vm1 / nbcol, vm1 mod nbcol
;;

(* h2 : taquin -> int
   Retourne la somme des distances de Manhattan des carreaux à leurs
   positions dans la solution
 *)
let h2 t =
  let g = t.grille in
  let dist = ref 0 in
  for i = 0 to nblig-1 do
    for j = 0 to nbcol-1 do
      let v = g.(i).(j) in
      if v <> 0 then
	let is, js = pos v in
	dist := !dist + abs (i - is) + abs (j - js)
    done
  done;
  !dist
;;

(* On veut maintenant écrire
   rech_chemin_borne_heuristique :
   (taquin -> int) -> taquin -> int -> int -> taquin list -> resultat
   L'appel (rech_chemin_simple_borne h t b n [s(n-1); ...; s(0)]) doit permettre
   de dire s'il existe un chemin simple commençant par
   [s(0); ...; s(n-1); s ], de longueur au plus b et arrivant à la
   solution.

   On implantera cette fonction comme précédemment, à deux différences
   près :

   * On échoue, lors du test initial, dès que n + h t > b ou t
     appartient à [s(n-1); ...; s(0)]

   * Si l'on échoue au moins une fois pour la première raison, on veut
     donner une information disant quelle est la plus petite valeur de
     n + h t qui dépassait b lors de la recherche. Cela sera utile
     pour effectuer des recherches par approfondissement successifs.

   Cela signifie en particulier que le type de retour ne sera plus
   (taquin list option) mais un type resultat, qu'on définit comme
   suit :
 *)
type resultat =
  | Solution of taquin list
  | Echec of int
;;
(* La valeur (Echec e) signifie qu'avec une valeur de la borne un peu
   plus grande, on aurait poussé l'exploration un peu plus loin.

  Lorsqu'on a des échecs uniquement en raison de conditions du type t
  appartient au chemin déjà parcouru, on retournera la valeur (Echec
  max_int).
*)
  
(* On peut maintenant écrire la fonction rech_chemin_borne_heuristique *)
  
(* À VOUS DE JOUER *)
let rec rech_chemin_borne_heuristique h t b n c =
;;

(* Essayez cette fonction avec h1 et h2 pour résoudre des taquins
   Regardez jusqu'à quelles profondeurs vous arrivez avec l'une et
   l'autre heuristique. *)

(* Comme précédemment, on peut faire de l'approfondissement.
   L'algorithme ainsi obtenu s'appelle IDA*, d'où le nom de la
   fonction ci-dessous.

   ida_star : (taquin -> int) -> taquin -> taquin list

   Effectue une recherche par approfondissement. Au lieu d'augmenter
   de un la profondeur de recherche à chaque appel, on va utiliser les
   informations sur l'échec pour savoir à quelle profondeur
   chercher : initialement, on cherche à profondeur 0 en utilisant la
   fonction rech_chemin_borne_heuristique. Si celle-ci retourne une
   solution, ida_star la retourne. Sinon, elle a retourné (Echec e) ce
   qui signifie qu'il est nécessaire que la borne de recherche soit
   supérieure ou égale à e pour qu'on puisse explorer de nouveaux
   chemins. On va donc effectuer une recherche avec la nouvelle borne
   de recherche e. Si celle-ci réussit, on retourne le résultat, sinon
   elle retourne (Echec e') et on retente une recherche avec la
   nouvelle borne e', etc.  
 *)

(* À VOUS DE JOUER *)
let ida_star h t =
;;
