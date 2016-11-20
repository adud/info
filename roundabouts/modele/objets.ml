(*les divers objets qui seront necessaires pour le programme*)


let p = 0.
;;
type distr = 
 	|Spawn
	|Quit of string
	|Int of intersection

 and intersection = {mutable ent:section list;mutable sor:section list;
 		     mutable qu:(voiture*int*section*section) list; transf: (voiture*int*section*section)-> unit}
 and voiture = {mutable spd:int;mutable dir:section list}
  
 and section = {mutable pre:distr;data:voiture option array;
			   maxspd:int;mutable post:distr}
;;

let observer sec = sec.data
;;

let radar c = c.spd
;;


let firstcar sec =
	let n = Array.length sec.data in
	let act = ref 0 in
	while !act < n && sec.data.(!act) = None do
		incr act
	done;
	!act
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

let creer_section sz ms = 
	let d = Array.make sz None in
	{pre=Spawn;post=Spawn;data=d;maxspd=ms}
;;

let creer_inter f = Int({ent=[];sor=[];qu=[];transf=f})
;;

(*lier : distr -> distr -> section -> unit
lier d1 d2 sec fait le lien de la distribution d1 à la distribution
d2 par la route sec*)

let lier d1 d2 sec =
	sec.pre <- d1;
	sec.post <- d2;
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
  while !act < n && sec.data.(!act) = None do
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
	  else
	    match c.dir,sec.post
	    with
	    |[],_ -> failwith "increment : objectiveless car"
	    |_,Spawn -> failwith "increment : arriver sur un depart"
	    |_,Quit(s) -> print_string s
	    |q::r,Int(inter) ->
	      let pat = c,n-(!act),sec,q in
	      inter.qu <- pat::inter.qu	    
	end
    end
;;

let traverser dst =
	match dst with
	|Int(isec) -> 
		List.iter isec.transf isec.qu
	|_-> failwith "traverser : passer a travers d'un debut ou d'une fin"
;;
	

(*comportements possibles aux intersections*)

let passif (c,d,e,s) = ()

;;

(*tentative sur un circuit*)

let circuit = {pre=Spawn;post=Quit("");maxspd=5;
			  data=Array.make 10 None}
;;
firstcar circuit;;

let checkpoint = Int({ent=[];sor=[];qu=[];transf=passif})
;;

lier checkpoint checkpoint circuit
;;

circuit.data.(0) <- Some({spd=4;dir=[circuit]});;
(*increment circuit;;
circuit.data;;
match checkpoint with
	|Int(i) -> i.qu
	|_ -> failwith "niet"
;;

*)


