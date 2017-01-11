(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements
;;
  
(*test two_one*)
let () = 
  let ent0 = creer_section 10 3 in
  let ent1 = creer_section 10 3 in
  let sor = creer_section 20 3 in
  let a = creer_spawn () in
  
  let bottle = creer_inter 2 (feux 10 10 0) in

  let grcr = [ent0,((20,480),-.pi/.2.,Graphics.red);
              ent1,((20,60),pi/.2.,Graphics.blue);
              sor, ((20,270),0.,Graphics.black)] in
  
  let itin = [sor;sor] in
  
  lier a 0 bottle 0 ent0;
  lier a 0 bottle 1 ent1;
  lier bottle 0 (creer_sortie "s") 0 sor;

  
  let s = [ent0;ent1;sor] in
  let sp0 = Plateau.spawn_car 3 2 0 0 ent0 itin in
  let sp1 = Plateau.spawn_car 3 1 0 0 ent1 itin in
  let p = Plateau.construire s [bottle] [sp0] in

  (*Plateau.jouer p 0 20;*)

  init 500 500;

  Plateau.animer p 0 100 grcr;

;;
