(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements


let checkpoint =  creer_inter 1 1 oneone
;; 

let circuit =  creer_section 166 6
;;

let rec tour = circuit::tour in

    
for i=0 to 4 do
  
  ajcar circuit (creer_voiture 6 tour) (i*7)

done;

for i=0 to 10 do
  ajcar circuit (creer_voiture 0 tour) (60 +i)
done
;;

  lier checkpoint 0 checkpoint 0
       circuit
;;

let () =
  for i=0 to (-1) do
    increment circuit;
    traverser checkpoint;
  done;
  for i=0 to 100 do
    print_section circuit;
    traverser checkpoint;
    increment circuit;  done;
;;
