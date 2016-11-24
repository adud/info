(*divers comportements aux intersections*)
open Objets
;;

let oneone (c,d,e,s) = 
  let check c = 
    print_int (radar c);
    print_string " ";
  in
  accel c ( panneau s);
  check c;
  desc c ( firstcar s - d);
  check c;
  descrand c p;
  check c;
  print_newline ();
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
