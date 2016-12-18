(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements


let checkpoint =  creer_inter 1 1 onemany
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

  lier checkpoint 0 checkpoint 0 circuit
;;
(*test circuit*)
let () =
  for i=0 to (-1) do
    increment circuit;
    traverser checkpoint i;
  done;
  for i=0 to (-1) do
    print_section circuit;
    traverser checkpoint i;
    increment circuit;  
  done;

(*test one_many*)

  let entr = creer_section 10 3 in
  let sor1 = creer_section 20 3 in
  let sor2 = creer_section 20 3 in
  let a = creer_spawn () in

  let trio = creer_inter 1 2 onemany in

  let itin1 = [sor1;sor1] in
  let itin2 = [sor2;sor2] in
  

  lier a 0 trio 0 entr;
  lier trio 0 (creer_sortie "1") 0 sor1;
  lier trio 1 (creer_sortie "2") 0 sor2;
  ajcar entr (creer_voiture 0 itin1) 0;
  ajcar entr (creer_voiture 0 itin2) 1;
  
  let s = [entr;sor1;sor2] in
  for i = 0 to (-10) do
    List.iter print_section s;
    print_newline ();
    List.iter increment s;
    traverser trio i;
  done;

(*test two_one*)
  let ent0 = creer_section 10 3 in
  let ent1 = creer_section 10 3 in
  let sor = creer_section 20 3 in
  let a = creer_spawn () in

  let bottle = creer_inter 2 1 (feux 6 6 0) in
  
  let itin = [sor;sor] in
  
  lier a 0 bottle 0 ent0;
  lier a 0 bottle 1 ent1;
  lier bottle 0 (creer_sortie "s") 0 sor;

  
  let s = [ent0;ent1;sor] in

  for i = 0 to 100 do
    if i mod 3 = 0
    then
      begin
	match (observer ent0).(0)
	with
	|None -> ajcar ent0 (creer_voiture 0 itin) 0
	|Some(_) -> ()
      end
    ;
    List.iter print_section s;
    print_newline ();
    List.iter increment s;
    traverser bottle i;
  done;
;;
