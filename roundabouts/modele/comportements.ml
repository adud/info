
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

let prioabs att ent sor = 
  match
    att.(0),att.(1)
  with
  |None,None -> ()
  |Some(c,d,e,s),None -> 
    if memq s sor && ent.(0) == e
    then internasch c d e s
    else failwith "prioabs_error 1: I/O non correspondantes"
  |None,Some(c,d,e,s) ->
    if memq s sor && ent.(1) == e
    then internasch c d e s
    else failwith "prioabs_error 2: I/O"
(*le cas interessant : deux voitures cherchent a traverser en meme temps*)
  |Some(cp,dp,ep,sp),Some(cl,dl,el,sl) ->
    if memq sp sor && ent.(0) == ep && memq sl sor && ent.(1) == el
    then 
      begin
	internasch cp dp ep sp;
	ajcar el cl (tsec el - 1);
      end
    else failwith "prioabs_error 3 :I/O"
;;

  

let passif att ent sor = ()
;;
