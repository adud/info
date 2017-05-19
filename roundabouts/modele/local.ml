(*etude locale de l'interet d'une intersection dynamique*)
open Objets
;;
open Affichage
;;
open Comportements
;;

(*infos :

d_p | v_p | J_p | d_np | v_np | J_np

faire dynamique et absolu
graphes interessants :
pente 0. 1. 
pente .2 .5
*)
  
  
(*test two_one*)
let nit = 10000
;;
  
let modele fout tint =
  let ent0 = creer_section 50 5 in
  let ent1 = creer_section 50 5 in
  let sor = creer_section 10 5 in
  let a = creer_spawn () in
  let s = creer_sortie "s" in
  
  let bottle = creer_inter 2 tint in

  let grcr = [ent0,((20,80),0.,Graphics.red);
              ent1,((30,20),0.08,Graphics.blue);
              sor, ((550,80),0.,Graphics.black)] in
  
  let itin = [sor;sor] in
  
  lier a 0 bottle 0 ent0;
  lier a 0 bottle 1 ent1;
  lier bottle 0 s 0 sor;
  lier bottle 1 s 0 sor;
  
  let s = [ent0;ent1;sor] in 
    
  (*let stable p t = p in*)
  let sp0 = Plateau.spawn_prog (pente 0. 1. nit) (panneau ent0) 0 ent0 itin in
  let sp1 = Plateau.spawn_car 5 0 (panneau ent1) 0 ent1 itin in
  let p = Plateau.construire s [bottle] [sp0;sp1] in
  let info t = ignore t;info_fdmal [ent0] @ info_fdmal [ent1] in


  let deb = false in
  if
    deb 
  then
    begin
      init 800 100;
      Plateau.animer p 0 nit grcr;
    end
  else
    Plateau.sauvegarder p 0 nit info fout;
;;

let () =
  redm := 2;
  modele "localabs.dat" prioabs;
  redm := 2;
  modele "localdyn.dat" priodyn;
;;
