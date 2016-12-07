(*les divers objets qui seront necessaires pour le programme*)


let p = 0.
;;
type distr = 
 	|Spawn
	|Quit of string
	|Int of intersection

 and intersection = {mutable ent:section array;mutable sor:section array;
 		     mutable qu:(voiture*int*section*section) list; transf: (voiture*int*section*section) -> section array -> section array -> unit}

(*si qu contient (v,d,e,s) c'est qu'une voiture v se trouvant a d (d>0 
de l'intersection, venant de e, allant vers s*)

 and voiture = {mutable spd:int;mutable dir:section list}
  
 and section = {mutable pre:distr;data:voiture option array;
			   maxspd:int;mutable post:distr}
;;


(*creer les objets*)

let creer_section sz ms = 
	let d = Array.make sz None in
	{pre=Spawn;post=Quit("");data=d;maxspd=ms}
;;

let dums = creer_section 0 0
;;
 

let creer_inter e s f = Int({ent=Array.make e dums;sor=Array.make s dums;qu=[];transf=f})
;;

let creer_spawn () = Spawn
;;

let creer_sortie s = Quit(s)
;;
  
let creer_voiture s d = {spd=s;dir=d}
;;

(*questionner les objets*)

let panneau sec = sec.maxspd
;;

let tsec sec = Array.length sec.data
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

let patients d = 
  match
    d
  with
  |Int(isec) -> isec.qu
  |_ -> failwith "patients : personne ne patiente pour entrer/sortir"
;;
(*manipuler les objets*)

(*lier : distr -> distr -> section -> unit
lier d1 d2 sec fait le lien de la distribution d1 à la distribution
d2 par la route sec*)

let lier d1 p1 d2 p2 sec =
	sec.pre <- d1;
	sec.post <- d2;
	match d1 with
	|Spawn -> ()
	|Quit(_) -> failwith "lier : entrer par une sortie"
	|Int(i1) -> i1.sor.(p1) <- sec
	;
	match d2 with
	|Spawn -> failwith "lier : sortir par une entree"
	|Quit(_) -> ()
	|Int(i2) -> i2.ent.(p2) <- sec
;;
  
let ajcar sec c pos =
	match sec.data.(pos) with
	|Some(_) -> failwith "ajcar : apparition d'une voiture sur une autre"
	|None -> sec.data.(pos) <- Some(c)

;;
(*iterations de l'automate*)

let accel c vmax =
  c.spd <- min (c.spd + 1) vmax
;;
let desc c dsec =
  c.spd <- min c.spd (dsec - 1)
;;
let descrand c p =
  if Random.float 1. <= p
  then c.spd <- max 0 (c.spd - 1)
;;
let move p sec =
  match sec.data.(p) with
  |None -> failwith "move :trying to move no car"
  |Some(c) ->
    if
      c.spd > 0
    then
      begin
	let d = p + c.spd in
	match sec.data.(d) with
	|Some(_) -> failwith "move : collision"
	|None ->
	  begin
	    sec.data.(d) <- Some(c);
	    sec.data.(p) <- None;	 
	  end
      end
    else ()
;;

(*increment: section -> unit 
met a jour la section avec les regles de l'automate cellulaire*)
  
let increment sec =
  let next = ref 0 in
  let act = ref 0 in
  let n = Array.length sec.data in
  act := firstcar sec;
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
	    (*la voiture suivante devient voiture actuelle*)
	    act := !next;
	    next := !act + 1;

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
	      inter.qu <- pat::inter.qu;
	      c.dir <- r;
	      sec.data.(!act) <- None
	end
    end
;;

let traverser dst =
	match dst with
	|Int(isec) -> 
		List.iter (fun (c,d,e,s) -> isec.transf (c,d,e,s) isec.ent isec.sor) isec.qu;
		isec.qu <- [];
	|_-> failwith "traverser : passer a travers d'un debut ou d'une fin"
;;


  
(*useless ?*)
let faire_sorties d n = 
  match
    d
  with
  |Spawn|Quit(_) -> failwith "ajouter_sorties : debut ou fin n'en a pas"
  |Int(isec) -> isec.sor <- Array.make n dums 
;;

let faire_entrees d n =
  match
    d
  with
  |Spawn|Quit(_) -> failwith "ajouter_sorties : debut ou fin n'en a pas"
  |Int(isec) -> isec.ent <- Array.make n dums
;;
