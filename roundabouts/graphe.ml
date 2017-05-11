(*etude du manege : theorie des graphes*)

type graphe = float array array
;;

let inf = 1./.0.
;;


(*crée un graphe pondere a n sommets sous forme matricielle*) 
let creer_graphe n = Array.make_matrix n n inf
;;

let taille g = Array.length g
;;
  
(*cree un arc de poids p de a a b dans g*)
let lier g a b p =
  g.(a).(b) <- p
;;

(*supprime l'arc a -> b
ne fait rien si l'arc n'existe pas*)
let suppr g a b = lier g a b inf
;;

let itere_succ f g s =
  for i=0 to taille g - 1 do
    if g.(s).(i) < inf
    then f i
  done
;;

(*implantation de l'algorithme de Floyd-Warshall*)

(*relache graphe -> int -> int -> int -> unit 
effectue une operation de relaxation elementaire sur w*)
let relache w a b k =
  let it = w.(a).(b) in
  let nit = w.(a).(k) +. w.(k).(b) in
  if nit < it then
    w.(a).(b) <- nit
;;

let floyd_warshall g =
  let n = taille g in
  let w = creer_graphe n in
  (*initialiser w*)

  for i=0 to (n-1) do
    for j=0 to (n-1) do
      w.(i).(j) <- g.(i).(j)
    done
  done;
  
  for i=0 to (n-1) do
    w.(i).(i) <- 0.
  done;

  for k = 0 to (n-1) do
    for i = 0 to (n-1) do
      for j = 0 to (n-1) do
        relache w i j k
      done
    done
  done;

  w
;; 

(*constantes pour les poids des aretes*)  
let pi = 4. *. atan 1.
;;

(* d'apres google maps : 60px = 10m *)
  
let r1,r2,r3 = 100.,40.,200.
  
let lcer = 2.*.pi*.r2/.3.
;;
let lint = 2.*.pi*.r1/.5.
;;
let lext = 2.*.pi*.r3/.5. -. lcer /. 2.
;;
  

(*creer_manege : float -> float -> float -> graphe*graphe
creer_manege lext lcer lint 
cree le manege et le rond-point associe pour les tailles de section donnees*)
  
let creer_manege lext lcer lint h =
  
  let mag = creer_graphe 20 in
  let rop = creer_graphe 20 in
  
  for i = 0 to 4 do
    let sor = 4*i in
    let pst = 4*i + 1 in
    let pre = 4*i + 3 in
    let ent = 4*i + 2 in
    let nxt = (4*i + 7) mod 20 in
    let ant = (4*i + 6) mod 20 in

    lier rop sor pst (lcer +. h);
    lier rop pre sor lcer;
    lier rop pre pst lcer;
    lier rop pst nxt lext;
    
    lier mag sor pst (lcer +. h);
    lier mag pre sor lcer;
    lier mag pst ent lcer;
    lier mag ent pre lcer;
    lier mag pre pst lcer;
    lier mag pst nxt (lext +. h);
    lier mag ant ent (lint +. h);

  done;
  mag,rop
;;

(*info_circ: graphe -> graphe
extrait d'un graphe de manege les lignes et les colonnes correspondant
a des entrees-sorties*)
  
let info_circ w =
  let s = creer_graphe 5 in
  for i=0 to 4 do
    for j = 0 to 4 do
      s.(i).(j) <- w.(i*4).(j*4)
    done
  done;
  s
;;

(*(++) float array array -> float array array -> float array array
somme de deux matrices de flottants
precondition : les deux matrices sont de memes dimensions*)
let ( ++ ) =
  Array.map2 (fun t1 t2 -> Array.map2 (fun x y -> x +. y) t1 t2)
;;

let opp  =
  Array.map (fun t -> Array.map (fun x -> -.x) t) 
;;

let alp =  [|
    [|2.;3.|];
    [|4.;5.|];
  |]
;;

(*h est un poids handicap pour les give-way*)
  
let res h =
  let mag,rop = creer_manege lext lcer lint h in
  opp (info_circ (floyd_warshall mag)) ++ (info_circ (floyd_warshall rop))
;;

(*somme des elements pour une matrice de flottants*)
  
let total m =
  Array.fold_left
    (fun x t -> x +. Array.fold_left (fun x y -> x +. y) 0. t) 0. m
;;

let dep_h i j = 
  for h=i to j do
    print_int h; print_char '\t';
    print_float (total (res (float_of_int h)));
    print_newline ();
  done
;;

(*fin de la partie FW, maintenant notion de section critique*)
(*transforme le graphe pondere represente par une matrice
en graphe oriente non-pondere represente par listes d'adjacences*)

let adj_of_mat g =
  let n = taille g in
  let s = Array.make n [] in
  for i=0 to n-1 do
    for j=n-1 downto 0 do
      if g.(i).(j) < inf then
        s.(i) <- j::s.(i)
    done
  done;
  s
;;

(*pas une tres bonne idee : on a besoin de supprimer des arcs*)
  
let parcours g a =
  let n = taille g in
  let vus = Array.make n false in
  let rec loop i =
    if not vus.(i) then
      begin
        vus.(i) <- true;
        itere_succ loop g i
      end
  in
  loop a;
  vus
;;
  
      
