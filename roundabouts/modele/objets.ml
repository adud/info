(*les divers objets qui seront necessaires pour le programme*)

let p = 0.

type distr = 
	|Spawn
	|Quit of string
	|Int of intersection

 and intersection = {mutable ent:section list;mutable sor:section list;
 					mutable qu:voiture list}
 and voiture = {mutable spd:int;mutable dir:distr list}
  
 and section = {pre:distr;data:voiture option array;
			   maxspd:int;post:distr}
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

(*lier : distr -> distr -> int -> int -> unit
lier d1 d2 s vm fait le lien de la distribution d1 à la distribution
d2 en creant une route vide de longueur s et de taille vm *)

let lier d1 d2 s vm =
	let vide = Array.make s None in
	let sec = {pre=d1;post=d2;maxspd=vm;
			   data=vide} in
	match d1 with
	|Spawn -> ()
	|Quit(_) -> failwith "lier : entrer par une sortie"
	|Int(i1) -> i1.sor <- sec::i1.sor
	;
	match d2 with
	|Spawn -> failwith "lier : sortir par une entree"
	|Quit(_) -> ()
	|Int(i2) -> i2.ent <- sec::i2.ent
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

let checkpoint = Int({ent=[];sor=[];qu=[]});;
lier checkpoint checkpoint 50 5;;
match checkpoint with Int(i) -> 
match i.sor with x::r -> x.data.(0) <- Some {spd=5;dir=[]};;

