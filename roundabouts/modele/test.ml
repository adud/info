(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements


let checkpoint =  creer_inter oneone
;; 

let circuit =  creer_section 80 6
;;

for i=0 to 10 do
  
  ajcar circuit (creer_voiture 0 [circuit]) (i)

done
;;

lier checkpoint checkpoint circuit
;;

let () =
  for i=0 to 15 do
    increment circuit;
    traverser checkpoint;
  done;
  for i=0 to 10 do
    print_section circuit;
    increment circuit;
    print_inter checkpoint;
    traverser checkpoint;
  done;
;;
