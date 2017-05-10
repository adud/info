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

(*constantes pour les poids des aretes*)  
let pi = 4. *. atan 1.
;;

let r1,r2,r3 = 100.,40.,200.
  
let lcer = 2.*.pi*.r2/.3.
;;
let lint = 2.*.pi*.r1/.5.
;;
let lext = 2.*.pi*.r3/.5. -. lcer /. 2.
;;
  

(*un poids handicap pour les give-way*)
let h = 10.
;;

(*creer_manege : float -> float -> float -> graphe*graphe
creer_manege lext lcer lint 
cree le manege et le rond-point associe pour les tailles de section donnees*)
  
let creer_manege lext lcer lint =
  
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
