(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements


let checkpoint =  creer_inter oneone
;; 

let circuit =  creer_section 70 6
;;

let rec tour = circuit::tour in
  
for i=0 to 10 do
  
  ajcar circuit (creer_voiture 0 tour) (i)

done
;;

lier checkpoint checkpoint circuit
;;

let () =
  for i=0 to (-1) do
    increment circuit;
    traverser checkpoint;
  done;
  for i=0 to 50 do
    print_section circuit;
    traverser checkpoint;
    increment circuit;  done;
;;
