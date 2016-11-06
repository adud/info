(*les divers objets qui seront necessaires pour le programme*)

let p = 0.

type entree =
  |Spawn
  |Ret of intersection
 and sortie =
   |Quit of string
   |Goto of intersection
 and intersection = {mutable ent:entree list;mutable sor: sortie list
 					mutable queue:voiture list}
 and voiture = {mutable spd:int;mutable dir:sortie list}
;;
  
type section = {pre:entree;data:voiture option array;
			   maxspd:int;post:sortie}
;;

  (*iterations de l'automate*)
(*accel : *)
let accel c vmax =
  c.spd <- min (c.spd + 1) vmax
;;
let desc c dsec =
  c.spd <- min c.spd dsec
;;
let descrand c p =
  if Random.float 1. <= p
  then c.spd <- max 0 (c.spd - 1)
;;
let move p sec =
  match sec.data.(p) with
  |None -> failwith "move :trying to move no car"
  |Some(c) ->
    let d = p + c.spd in
    match sec.data.(d) with
    |Some(_) -> failwith "move : collision"
    |None ->
      begin
	sec.data.(d) <- Some(c);
	sec.data.(p) <- None;	 
      end
;;
  
	     
(*increment: section -> unit *)
  
let increment sec =
  let next = ref 0 in
  let act = ref 0 in
  let n = Array.length sec.data in
  while sec.data.(!act) = None && !act < n do
    incr act;
  done;
  if !act < n then (*la section n'est pas vide*)
    begin
      next := !act+1;
      while !next < n do
	match sec.data.(!next) with
	|None-> incr next
	|Some(vdev) ->
	  match sec.data.(!act) with
	  |None -> failwith "increment : empty car"
	  |Some(vder) ->
	    (*on vient de determiner la voiture actuelle et la voiture de devant*)
	    accel vder sec.maxspd;
	    desc vder (!next - !act);
	    descrand vder p;
	    move !act sec;
      done;
    (*sec.data.(!act) contient la derniere voiture de la section qui peut 
     quitter cette derinere lors de l'iteration*)
      match sec.data.(!act) with
      |None -> failwith "increment : empty lastcar"
      |Some(c) ->
	begin
	  accel c sec.maxspd;
	  if !act + c.spd < n
	  then move !act sec
	  else print_string "bye"
	end
    end
;;

(*tentative sur un circuit*)
let cond_init = Array.make 10 None
;;
cond_init.(0) <- Some {spd=3;dir=[]};;
let rec boucle  = {pre=depart;post=arrivee;maxspd=5;data=cond_init}
and depart = Ret(checkpoint)
and arrivee = Goto(checkpoint)
and checkpoint = {ent=[];sor=[arrivee]};;



