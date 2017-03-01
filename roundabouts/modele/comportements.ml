
(*divers comportements aux intersections*)
open Objets
;;
let amemq x t = Array.fold_left (fun b a -> (a == x)||b) false t
;;

let corresp e s ent sor i =
  List.memq s sor.(i) && ent.(i) == e
  
(*modelise Nagel-Schreckenberg pour un carrefour*)

let refuse c e s =
  brake c;
  ajdir c s;
  ajcar e c (tsec e - 1);

;;

let internasch c d e s =
  accel c ( panneau s);
  desc c ( firstcar s + d);
  descrand c !p;

  let pos = radar c - d in
  if
    pos < 0
  then
    refuse c e s
  else
    ajcar s c pos
;;
  
let onemany att ent sor t =
  match
    att.(0)
  with
  |Some(c,d,e,s) ->
    if
      corresp e s ent sor 0
    then
      internasch c d e s 
    else
      failwith "onemany_error : sorties non correspondantes"
  |None -> ()

;;

let twomany cpm att ent sor t = 
  match
    att.(0), att.(1)
  with
  |None,None -> ()
  |Some(c,d,e,s),None -> 
    if corresp e s ent sor 0
    then internasch c d e s
    else failwith "twomany_error 1: I/O non correspondantes"
  |None,Some(c,d,e,s) ->
    if corresp e s ent sor 1
    then internasch c d e s
    else failwith "twomany_error 2: I/O"
(*le cas interessant : deux voitures cherchent a traverser en meme temps*)
  |Some(cp,dp,ep,sp),Some(cl,dl,el,sl) ->
    if  corresp ep sp ent sor 0 && corresp el sl ent sor 1
    then
      cpm cp dp ep sp cl dl el sl
    else failwith "prioabs_error 3 :I/O"


let absolu cp dp ep sp cl dl el sl = 
  internasch cp dp ep sp;
  refuse cl el sl;
;;

let dynamique cp dp ep sp cl dl el sl =
  let temps c d s = (*le temps necessaire pour arriver a l'intersection*)
    (*denominateur : estimation du resultat Nasch sans alea*)
    float_of_int d /. float_of_int (min (panneau s) (min (d + firstcar s) (radar c +1)))
  in
  let priop cp dp sp cl dl sl = 
    let tp,tl = (temps cp dp sp),(temps cl dl sl) in 
    tp < tl || (tp = tl && dp <= dl)
  in
  if priop cp dp sp cl dl sl 
  then
    begin
      internasch cp dp ep sp;
      internasch cl dl el sl;
    end
  else
    begin
      internasch cl dl el sl;
      internasch cp dp ep sp
    end
;;
let prioabs att ent sor t = twomany absolu att ent sor t
;;

let priodyn att ent sor t = twomany dynamique att ent sor t
;;

let feux dur1 dur2 ph att ent sor t =
  let t' = (t - ph) mod (dur1 + dur2) in
  let passer vert i =
    match
      att.(i)
    with
    |None -> ()
    |Some(c,d,e,s) ->
      if corresp e s ent sor i
      then
	if
	  vert
	then
	  internasch c d e s
	else
	  refuse c e s
      else
	failwith "feux error : I/O"
  in
  passer (t' < dur1) 0;
  passer (t' >= dur1) 1;
  
;;

let passif att ent sor t= ()
;;


  (*comportements d'apparition de voitures*)

let pente b e s t =
  b +. float_of_int t *. (e -. b) /. float_of_int s 
