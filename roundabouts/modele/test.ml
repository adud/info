(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;

 
let oneone (c,d,e,s) = 
  accel c ( panneau s);
  desc c ( firstcar s - d);
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

let checkpoint =  creer_inter oneone
;; 

let circuit =  creer_section 40 5
;;

for i=0 to 12 do
  
  ajcar circuit (creer_voiture 0 [circuit]) (3*i)

done
;;

lier checkpoint checkpoint circuit
;;

let () =
  print_section circuit;
  for i=0 to 20 do
    increment circuit;
    traverser checkpoint;
    print_section circuit;
  done;
  increment circuit;
print_section circuit;
traverser checkpoint;
print_section circuit;
;;
