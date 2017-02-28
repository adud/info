(*etude des limites du modele : regarder l'état
du diagramme pour de grandes valeurs de densité*)

(*
./sature > truc.dat
gnuplot
set xlabel "<d>"
set ylabel "<J>"

plot [0:0.2] [0:1] "truc.dat" using 1:3
 *)

open Objets
;;
open Affichage
;;
open Comportements
;;
  

let () = 
  let ent = creer_section 50 5 in
  
  let grcr = [ent,((20,80),0.,Graphics.red);]; in
  
  let itin = [ent] in
  
  let s = [ent] in
  let gr t = (float_of_int t) /. 2000. +. 0.5 in
  
  let sp = Plateau.spawn_prog gr (panneau ent) 0 ent itin in
  let p = Plateau.construire s [] [sp] in
  let info t = ignore t; (info_fdmal [ent]) in
  
  let deb = false in

  Plateau.silence p 0 100;
  if
    deb 
  then
    begin
      init 800 100;
      Plateau.animer p 0 1000 grcr;
    end
  else
    Plateau.modeliser p 0 1000 info;

;;
