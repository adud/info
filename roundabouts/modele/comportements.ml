(*divers comportements aux intersections*)
open Objets
;;
let mem x t = Array.fold_left (fun b a -> (a = x)||b) false t
;;
  
let oneone (c,d,e,s) ent sor = 
  (*print_string "circuit enre : ";
  Affichage.print_section s;*)
  if not (ent.(0) == e && sor.(0) == s)
  then
    failwith "oneone_error : entrees ou sorties non correspondantes"
  ;
  accel c ( panneau s);
  desc c ( firstcar s + d);
  descrand c p;

  let pos = radar c - d in
  if
    pos < 0
  then
    ajcar e c (tsec e - 1)
  else
    ajcar s c pos
;;

let passif (c,d,e,s) ent sor = ()
;;
