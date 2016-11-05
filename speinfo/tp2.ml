type noeud = 
	|Feuille of bool
	|Decision of string*int*int
	|Vide
;;

let monAD = 
[|Decision("e",1,2);
 Feuille true;
 Decision("a",3,4);
 Decision("r",5,6);
 Feuille false;
 Feuille true;
 Feuille false
|]
;;

let exemple = 
[|Decision("z1",1,2);
  Decision("z2",3,4);
  Decision("z2",5,6);
  Decision("z3",7,8);
  Decision("z3",9,10);
  Decision("z3",11,12);
  Decision("z3",13,14);
  Feuille true;
  Feuille false;
  Feuille true;
  Feuille false;
  Feuille true;
  Feuille false;
  Feuille false;
  Feuille false;
|]
;;

let rec eval_var k l = match l with
	|x::r -> k=x||eval_var k r
	|[] -> false
;;

let eval_var = mem;;

(*eval_aux: int -> noeud vect -> string list -> bool
evalue l'arbre de decision arb pour la valuation v
a partir de la position pos*)

let rec eval_aux pos arb v =
	match arb.(pos) with
	|Vide -> failwith "eval_aux : tombe sur un noeud vide !"
	|Feuille(b) -> b
	|Decision(i,t,e) ->
		if eval_var i v
		then eval_aux t arb v 
		else eval_aux e arb v
;;

let eval = eval_aux 0
;;

let redirige gr v w =
	let n = vect_length gr in
	gr.(v) <- Vide;
	for j=0 to n-1 do
		match gr.(j) with
		|Feuille(_)|Vide -> ()
		|Decision(i,t,e) -> 
			let t' = if t = v then w else t in
			let e' = if e = v then w else e in
			gr.(j) <- Decision(i,t',e')
		;
	done
	;;


let trouve_elimination gr = 
	let n = vect_length gr in
	let c = ref 0 in
	let j = ref (-1) in
	while !j = (-1) && !c < n do
		begin
			match gr.(!c) with
			|Feuille(_)|Vide -> ()
			|Decision(_,t,e) -> 
				if gr.(t) = gr.(e) then (j := !c)
		end;
		incr c
	done;
	!j
;;

trouve_elimination exemple;;

let trouve_isomorphisme gr =
	let n = vect_length gr in 
	let j = ref (-1,-1) in
	let c1,c2 = ref 0,ref 1 in
	while !c1 < n  && !j = (-1,-1) do
		while !c2 < n && !j = (-1,-1) do
			begin
			match gr.(!c1),gr.(!c2) with
			|Vide,_ | _,Vide | Decision(_,_,_),Feuille(_)
			|Feuille(_),Decision(_,_,_) -> ()
			|Feuille(b1),Feuille(b2) ->
				if b1 = b2 then j := !c1,!c2
			|Decision(i1,t1,e1),Decision(i2,t2,e2) ->
				if i1 = i2 && gr.(t1) = gr.(t2) && gr.(e1) = gr.(e2)
				then j := !c1,!c2
			end;
		incr c2
		done;
	incr c1;
	c2 := !c1 + 1
	done;
	!j
;;

trouve_isomorphisme exemple;;

let reduit gr = 
	let j,k = ref (-1),ref (-1,-1) in
	let s = ref true in
	while !s do
		s := false;
		j := trouve_elimination gr;
		if !j <> (-1) then 
		begin
			match gr.(!j) with
			|Decision(_,t,_) ->
				redirige gr !j t;
			| _ -> failwith "reduit : elimination appelee sur autre chose qu'un noeud"
			s := true;
		end
		;
		k := trouve_isomorphisme gr;
		if !k <> (-1,-1) then
		begin
			let fst,snd = !k in
			redirige gr fst snd;
			s := true
		end
		;
	done
;;



(*III

e1 -> 0,1 == !(e1)
e1 -> e1,e2 == e1 || e2
e1 -> e2,e1 == e1 && e2

*)



