
(*divers comportements aux intersections*)
open Objets
;;
let memq x t = Array.fold_left (fun b a -> (a == x)||b) false t
;;

(*modelise Nagel-Schreckenberg pour un carrefour*)

let internasch c d e s =
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
  
let onemany att ent sor =
  match
    att.(0)
  with
  |Some(c,d,e,s) ->
    if
      memq s sor && ent.(0) == e
    then
      internasch c d e s 
    else
      failwith "onemany_error : sorties non correspondantes"
  |None -> ()

;;
let passif att ent sor = ()
;;
