(*sert a tester différentes fonctions*)
open Objets
;;
open Affichage
;;
open Comportements
;;
open Plateau
  
(*test rond-point*)
let () =
  let rond = creer_rond_point 4 10 5 3 4 prioabs in
  let ent = (rp_ent rond).(0) in
  let sor = (rp_sor rond).(2) in
  let itin =
    match faire_itin rond 0 2 []
    with
    |r::q ->q
    |_ -> failwith "merdre"
  in
  let sp = spawn_car 5 0 0 0 ent itin in

  let pl = construire [] [] [sp] in
  
  pl_add_rp pl rond;
  
  jouer pl 0 10 
;;
