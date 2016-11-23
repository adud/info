open Objets
;;
open Affichage
;;
let circuit = creer_section 40 5
;;

let () =
print_section circuit;
ajcar circuit (creer_voiture 0 [circuit]) 0;
print_section circuit;

;;
