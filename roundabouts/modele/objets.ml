(*les divers objets qui seront necessaires pour le programme*)


let p = 0.
;;
type distr = 
 	|Spawn
	|Quit of string
	|Int of intersection

 and intersection = {mutable ent:section array;mutable sor:section list array;
 		     mutable att:(voiture*int*section*section) option array; transf: (voiture*int*section*section) option array-> section array -> section list array -> int -> unit}

(*si att.(x) contient (v,d,e,s) c'est qu'une voiture v se trouvant a d (d>0 
de l'intersection, venant de e, allant vers s ou x est la case de att reservee a e*)

 and voiture = {mutable spd:int;mutable dir:section list;nom:string}
  
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
 

let creer_inter n f = Int({ent=Array.make n dums;sor=Array.make n [];att=Array.make n None;transf=f})
;;

let creer_spawn () = Spawn
;;

let creer_sortie s = Quit(s)
;;
  
let creer_voiture s d n = {spd=s;dir=d;nom=n}
;;

let itere_voitures f sec =
  let g sc =
    match
      sc
    with
    |Some(c) -> f c;
    |None -> ();
  in
  Array.iter g sec.data
;;
  
(*questionner les objets*)
let who c = c.nom
;;
  
let rec somme f ls =
  match ls with
  |[] -> 0
  |x::r -> (f x) + somme f r
;;

let panneau sec = sec.maxspd
;;

let tsec sec = Array.length sec.data
;;

let observer sec = sec.data
;;
let survoler sec =
  let f x =
    match
      x
    with
    |Some(_) -> true
    |None -> false
  in
  Array.map f sec.data
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
  |Int(isec) -> isec.att
  |_ -> failwith "patients : personne ne patiente pour entrer/sortir"
;;
(*etudier les objets : outils de mesure*)

let nombre_voitures sec =
  let co = ref 0 in
  let f sc =
    match
      sc
    with
    |Some(c) -> incr co
    |None -> ()
  in
  Array.iter f sec.data;
  !co
;;

let nombre_voitures sec =
  let c = ref 0 in
  let f x = incr c in
  itere_voitures f sec;
  !c
;;

let somme_vitesses sec =
  let c = ref 0 in
  let f x = c:= radar x + !c in
  itere_voitures f sec;
  !c
  
let densite lsec =
  let nb = somme nombre_voitures lsec in
  let taille = somme tsec lsec in
  (float_of_int nb) /. (float_of_int taille)
;;

let vitesse_moy lsec =
  let nb = somme nombre_voitures lsec in
  let vts = somme somme_vitesses lsec in
  (float_of_int vts) /. (float_of_int nb)
;;
  
let flot_moy lsec =
  (densite lsec) *. (vitesse_moy lsec)
;;
  

(*manipuler les objets*)


let lier d1 p1 d2 p2 sec =
	sec.pre <- d1;
	sec.post <- d2;
	begin
	  match d1 with
	  |Spawn -> ()
	  |Quit(_) -> failwith "lier : entrer par une sortie"
	  |Int(i1) -> i1.sor.(p1) <- sec::i1.sor.(p1)
	end
	;
	begin
	  match d2 with
	  |Spawn -> failwith "lier : sortir par une entree"
	  |Quit(_) -> ()
	  |Int(i2) -> i2.ent.(p2) <- sec
	end
;;
  

let ajcar sec c pos =
	match sec.data.(pos) with
	|Some(_) -> failwith "ajcar : apparition d'une voiture sur une autre"
	|None -> sec.data.(pos) <- Some(c)

;;

let ajcar_sil sec c pos =
  try
    ajcar sec c pos
  with
  |Failure "ajcar : apparition d'une voiture sur une autre"
   -> ()
(*iterations de l'automate*)

let brake c =
  c.spd <- 0
;;
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

let ajdir c s =
  c.dir <- s::c.dir
;;

let indexq x ar =
  let n = Array.length ar in
  let r = ref 0 in
  while !r < n && ar.(!r) != x
  do incr r
  done;
  if !r = n
  then raise Not_found
  else !r
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
	    |_,Spawn -> failwith "increment : arriver sur un depart"
	    |_,Quit(s) -> 
	      begin
		(*print_string s;
		print_newline ();*)
		sec.data.(!act) <- None;
	      end
	    |[],_ -> failwith "increment : objectiveless car"
	    |q::r,Int(inter) ->
	      begin
		let pat = c,n-(!act),sec,q in
		let v = indexq sec inter.ent in
		inter.att.(v) <- Some(pat);
		c.dir <- r;
		sec.data.(!act) <- None;
	      end
	end
    end
;;

let traverser dst t =
	match dst with
	|Int(isec) -> 
	  isec.transf isec.att isec.ent isec.sor t;
	  Array.fill isec.att 0 (Array.length isec.att) None;
	|_-> failwith "traverser : passer a travers d'un debut ou d'une fin"
;;
