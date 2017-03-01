(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements
;;
open Plateau
;;

  
let () =
  let nit = 1000 in
  
  let ent0 = creer_section 50 5 in
  let ent1 = creer_section 50 5 in
  let sor = creer_section 50 5 in
  let a = creer_spawn () in
  let s = creer_sortie "s" in
  
  let bottle = creer_inter 2 (feux 10 10 0) in

  let grcr = [ent0,((20,80),0.,Graphics.red);
              ent1,((20,20),0.,Graphics.blue);
              sor, ((20,50),0.,Graphics.black)] in
  
  let itin = [sor;sor] in
  
  lier a 0 bottle 0 ent0;
  lier a 0 bottle 1 ent1;
  lier bottle 0 s 0 sor;
  lier bottle 1 s 0 sor;
  
  let s = [ent0;ent1;sor] in 
    
  (*let stable p t = p in*)
  let sp0 = Plateau.spawn_car 4 0 (panneau ent0) 0 ent0 itin in
  let sp1 = Plateau.spawn_car 5 0 (panneau ent1) 0 ent1 itin in
  let p = Plateau.construire s [bottle] [sp0;sp1] in
  let info t = ignore t;info_fdmal [ent0] @ info_fdmal [ent1] in


  let deb = true in
  if
    deb 
  then
    begin
      init 800 100;
      Plateau.animer p 0 nit grcr;
    end
  else
    Plateau.modeliser p 0 nit info;
;;

