(*divers comportements aux intersections*)
open Objets
;;

let oneone (c,d,e,s) = 
  (*print_string "circuit enre : ";
  Affichage.print_section s;*)
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

let passif (c,d,e,s) = ()
;;
