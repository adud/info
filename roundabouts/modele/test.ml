(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements


let checkpoint =  creer_inter oneone
;; 

let circuit =  creer_section 10 6
;;

for i=0 to 9 do
  
  ajcar circuit (creer_voiture 0 [circuit]) (i)

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
    increment circuit;
    print_string "circuit reel : ";
    print_section circuit;
    print_string "traversant   : ";
    print_inter checkpoint;
    traverser checkpoint;
    print_newline ();
  done;
;;
