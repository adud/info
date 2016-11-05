let param = 3
;;

type ('k,'v) btree = {
  mutable keys:'k vect;
  mutable size:int;
  mutable vals:('k,'v) intext;
}

(*les infos associÃ©es aux clÃ©s : size valeurs ou size sous-arbre*)
 and ('k,'v) intext =
   |Values of 'v vect
   |Sons of ('k,'v) btree vect
;;
(*empty : unit -> ('k,'v) btree
arbre vide*)
let empty () = {
  size = 0;
  keys = [||];
  vals = Values [||];
}
;;
  
(* insert_vect n u i v
dÃ©cale les Ã©lÃ©ments u[i],...,u[n-1] d'un cran vers la droite et remplace u[i]
par v*)
(*let insert_vect n u i v =
  for k = n downto i+1 do
    u.(k) <- u.(k-1);
  done;
  u.(i) <- v
;;
*)

let insert_vect n u i v =
	blit_vect u i u (i+1) (n-i);
	u.(i) <- v
;;


(*is_full a renvoie true si a est plein*)
let is_full a = a.size == 2*param
;;

(*lookup t n k retourne le plus petit indice i < n tq k <= t.(i) ou n si un tel i
n'existe pas. les n premiers Ã©lÃ©ments de t sont supposÃ©s triÃ©s par ordre croissant*)
let lookup t n k =
  let j = ref 0 in
  while
    !j < n && k > t.(!j) do
    incr j
  done;
  !j
;;

let rec find a k =
  match a.vals with
  |Values(t) ->
    let p = lookup a.keys a.size k in (*vect_length k = vect_length t = a.size*)
    if p < a.size && a.keys.(p) = k then t.(p)
    else raise Not_found
  |Sons(s) ->
    let p = lookup a.keys (a.size-1) k in
    find s.(p) k
;;
let a = {keys=[|1;2;4;8;16;42|];size=6;vals=Values([|1;2;3;4;5;8|])};;


(*rhalf 'a vect -> 'b vect -> ('a, 'b) btree * 'a
si cl et it sont resp. un un tableau de clefs et l'intext d'un
noeud N de param elements, alors rhalf cl it retourne le noeud
constitue de la moitie droite (la plus grande) de N ainsi que la cle
separant la moitie gauche de la moitie droite*)


let rhalf cl it = match it with
	|Values(t) -> 
		let clefs = make_vect (param*2) cl.(0) in
		let valeurs = make_vect (param*2) t.(0) in
		(*for i=0 to param-1 do
			clefs.(i) <- cl.(param+i);
			valeurs.(i) <- t.(param+i);
		done;*)
		blit_vect cl param clefs 0 param;
		blit_vect t param valeurs 0 param;
		let cnv = cl.(param-1) in
		let vnv = {size=param;keys=clefs;vals=Values(valeurs)} in
		vnv,cnv
		
	|Sons(s) -> 
		let clefs = make_vect (param*2-1) cl.(0) in
		let valeurs = make_vect (param*2) (empty ()) in
		(*ajoute les moities*)
		(*for i=0 to param-2 do
			clefs.(i) <- cl.(param+i)
		done;
		for i=0 to param-1 do
			valeurs.(i) <- s.(param+i);
		done;*)
		blit_vect cl param clefs 0 (param-1);
		blit_vect s param valeurs 0 param;
		(*cree le deuxieme fils*)
		let cnv = cl.(param-1) in
		let vnv = {size=param; keys=clefs; vals=Sons(valeurs)} in
		vnv,cnv
;;

let b = {size=6;keys=[|1;2;4;5;6;7|];vals=Values([|1;2;3;4;5;6|])};;
rhalf [|1;2;4;8;16|] (Sons [|empty ();empty ();empty ();b;empty ();b|]);;

(*ajoute la liaison (k,v) a l'arbreB a.
Effectue les partages des noeuds pleins rencontres lors de son passage pour
etre sur d'etre en mesure d'effectuer l'insertion demandee.
Precondition : le noeud racine de a n'est pas plein*)

let rec add_aux a k v = match a.vals with
	|Values t -> 
		let i = lookup a.keys a.size k in
		insert_vect a.size a.keys i k;
		insert_vect a.size t i v;
		a.size <- a.size + 1;
		(*ne sera pas applique a un tableau plein grace au code suivant*)
	|Sons s -> 
		let i = lookup a.keys (a.size-1) k in
		let fils = s.(i) in
		if is_full fils then
			(*partage du noeud*)
			begin
				fils.size <- param;
				let vnv,cnv = rhalf fils.keys fils.vals in
				insert_vect (a.size-1) a.keys i cnv;
				insert_vect a.size s (i+1) vnv;
				a.size <- a.size + 1;
					(*on a coupe le noeud en 2, on peut continuer la recherche*)
				if k <= cnv then	
					add_aux fils k v
				else
					add_aux vnv k v
			end
		else
			add_aux fils k v
;;


let add a k v = 
	if a.size = 0 then
		let clefs = make_vect (param*2) k in
		let valeurs = make_vect (param*2) v in
		a.keys <- clefs;
		a.vals <- Values(valeurs);
		a.size <- 1
	else if is_full a then
		begin
			let vd,cnv =	rhalf a.keys a.vals in
			let vg = {size=param;keys=a.keys;vals=a.vals} in
			(*on a coupe le noeud en 2, on peut continuer la recherche*)
			a.size <- 2;
			a.keys <- make_vect (param*2-1) cnv;
			let sons = make_vect (param*2) (empty ()) in
			sons.(0) <- vg;
			sons.(1) <- vd;
			a.vals <- Sons sons;
			add_aux a k v;
		end
	else add_aux a k v
;;

let rec size a = match a.vals with
	|Values(t) -> a.size
	|Sons(s) -> let j = ref 0 in
		for k=0 to (a.size-1) do
			j := !j + size s.(k)
		done;
		!j
;;

(*la suppression commence ICI*)

(* is_slim : ('a, 'b) btree -> bool
renvoie true si le noeud a besoin d'etre nourri*)
let is_slim a = a.size < param;; 

(*pop_vect: int -> 'a vect -> int -> 'a 
reciproque de la fonction insert_vect
pop n u i retourne la valeur i de u avant de decaler
les i+1 a (n-1) elements vers la gauche pour 'combler le trou' *)

let pop_vect n u i =
	let p = u.(i) in
	blit_vect u (i+1) u i (n-i-1);
	p
;;

(*remove int -> 'a vect -> int -> unit
idem pop_vect mais ne recupere pas la valeur retiree*)

let remove n u i = let _=pop_vect n u i in ()
;;

(*leechseed : ('a, 'b) btree -> int -> unit
leechseed a n : si a = Sons(s) alors le noeud s.(n) va drainer une valeur
au noeud s.(n+1) : la premiere valeur de s.(n+1) va devenir la derniere
valeur de s.(n)*)

let leechseed a n = 
	match a.vals with
	|Values(_) -> failwith "leechseed Error : leaves"
	|Sons(s) ->
		let vam,vic = s.(n),s.(n+1) in 
	match vam.vals , vic.vals with
	|Values(_),Sons(_)
	|Sons(_),Values(_) -> failwith "leechseed Error : not a B tree"
	|Values(v1),Values(v2) ->
		let clef = pop_vect vic.size vic.keys 0 in (*le drain*)
		let valeur = pop_vect vic.size v2 0 in
		vic.size <- vic.size-1; 
		vam.keys.(vam.size) <- clef; (*la recuperation*)
		v1.(vam.size) <- valeur;
		vam.size <- vam.size+1;
		a.keys.(n) <- clef; (*reindexation du noeud*)
		
	|Sons(s1),Sons(s2) ->
		let clef = pop_vect vic.size vic.keys 0 in
		let valeur = pop_vect vic.size s2 0 in
		vic.size <- vic.size - 1;
		vam.keys.(vam.size-1) <- a.keys.(n);
		s1.(vam.size) <- valeur;
		vam.size <- vam.size + 1;
		a.keys.(n) <- clef
;;
(*leechseed_inv : ('a, 'b) btree -> int -> unit
meme effet que leechseed mais du noeud s.(n-1) vers le noeud s.(n)
si plus elegant, suis preneur*)

let leechseed_inv a n =
	match a.vals with
	|Values(_) -> failwith "leechseed inv Error : leaves"
	|Sons(s) ->
		let vam,vic = s.(n),s.(n-1) in
	match vam.vals,vic.vals with
	|Values(_),Sons(_)
	|Sons(_),Values(_) -> failwith "leechseed inv Error : not a B tree"
	|Values(v1),Values(v2) ->
		let clef = pop_vect vic.size vic.keys (vic.size-1) in (*le leechseed du DERNIER element*)
		let valeur = pop_vect vic.size v2 (vic.size-1) in
		vic.size <- vic.size - 1;
		insert_vect vam.size vam.keys 0 clef;(*recuperation*)
		insert_vect vam.size v1 0 valeur;
		vam.size <- vam.size+1;
		a.keys.(n-1) <- vic.keys.(vic.size-1);(*reindexation du noeud*)
	
	|Sons(s1),Sons(s2) ->
		let clef = pop_vect vic.size vic.keys (vic.size-2) in
		let valeur = pop_vect vic.size s2 (vic.size-1) in
		vic.size <- vic.size - 1;
		insert_vect (vam.size-1) vam.keys 0 a.keys.(n-1);
		insert_vect (vam.size) s1 0 valeur;
		vam.size <- vam.size + 1;
		a.keys.(n) <- clef (*reindexation du noeud*)
;;

(*absorb : ('a, 'b) btree -> int -> unit 
si a.vals est un Sons(s) alors s.(n) absorbe s.(n+1) :
de deux noeuds il n'en reste qu'un*)
		
let absorb a n =
	match a.vals with
	|Values(_) -> failwith "Absorb Error : leaves"
	|Sons(s) -> 
		let vam,vic = s.(n),s.(n+1) in
		for i=0 to vic.size-1 do
			leechseed a n
		done;
		remove a.size s (n+1);
		remove (a.size-1) a.keys n;
		a.size <- a.size - 1;
		()
;;
(*suppr_aux :('a, 'b) btree -> 'a -> unit 
suppr_aux a k va supprimer l'élément k de a tout en
equilibrant preventivement l'arbre,
n'est pas valable sur la racine*)

let rec suppr_aux a k = 
	match a.vals with
	|Values(v) -> 
		let i = lookup a.keys a.size k in
		if i< a.size && a.keys.(i)=k then
		begin
			remove a.size a.keys i;
			remove a.size v i;
			a.size <- a.size - 1;
		end
		else raise Not_found
	|Sons(s) ->
		let i = lookup a.keys (a.size-1) k in
		let fils = s.(i) in
		if is_slim fils then 
		begin
			if i < a.size-1 then (*n'est pas le fils gauche*)
			begin
				if s.(i+1).size <= param then absorb a i
				else leechseed a i
			end
			else
			begin
				if s.(i-1).size <= param then absorb a (i-1)
				else leechseed_inv a i
			end
		end else ();
		suppr_aux fils k
;;
(*suppr : ('a, 'b) btree -> 'a -> unit
supprime l'element de cle k de a et diminue
de 1 la hauteur de l'arbre si la racine est un
Sons(s) qui n'a qu'un fils*)
let suppr a k =
	if a = empty () then failwith "Suppr Error : Empty"
	else
		begin
		suppr_aux a k;
		if a.size = 1 then
			match a.vals with
			|Values(v) -> ()
			|Sons(s) ->
				a.size <- s.(0).size;
				a.keys <- s.(0).keys;
				a.vals <- s.(0).vals;
		end
;;

(*Tests*)

(*assert : bool -> unit
  assert condition : lève une exception si une exception n'est pas vérifiée.*)
let assert b = if not b then failwith "Assertion Error";;

(*test de la fonction insert_vect :*)
let vect = [|0; 1; 2; 3; -1|] in
insert_vect 4 vect 1 42;
assert (vect = [|0; 42; 1; 2; 3|]);;

(*test de la fonction pop_vect :*)
let vect = [|0; 42; 1; 2; 3|] in
let a = pop_vect 5 vect 1 in
assert ((a,vect) = (42,[|0;1;2;3;3|]));;

(*test de la fonction lookup :*)
let vect = [|2; 3; 5; 8; 13|] in
assert (lookup vect 5 3 = 1);
assert (lookup vect 5 9 = 4);
assert (lookup vect 5 30 = 5);;

(*test de la fonction rhalf :*)
let a = {keys=[|1; 2; 3; 4; 5; 6|];
         size=6;
         vals= Values [|1; 2; 3; 4; 5; 6|]} in
let s2,k = rhalf a.keys a.vals in
assert (3 <= k && k <= 4);
assert ((s2.keys.(0), s2.keys.(1), s2.keys.(2)) = (4, 5, 6));;

(*test des fonctions add et find :*)

(*keysvalues : une liste de 100 couples (k, v) avec
  k, v les futurs clés et valeurs de l'arbre.
  Avec 100 clés entre 0 et 999999, il y a 99.5% de chance
  qu'il n'y ait aucune clé en double, ce qui poserait des problèmes
  pour find.*)
let keysvalues =
  let rec create x = match x with
    |0 -> []
    |_ -> (random__int 1000000, random__int 1000000) :: create(x-1)
  in
  create 100
;;

let a = empty();;
do_list (fun (k, v) -> add a k v) keysvalues;;
do_list (fun (k, v) -> assert(find a k = v)) keysvalues;;


a;;
find a 249071;;
suppr a 249071;;
find a 249071;;
(*test de la fonction leechseed*)

let a = {keys=[|1; 2; 3; 42; 50; 60|];
         size=3;
         vals= Values [|2; 4; 8; 16; 24; 32|]} in
let a' = {keys=[|4; 5; 6; 4; 5; 6|];
         size=3;
         vals= Values [|2; 4; 8; 16; 24; 32|]} in 

let e = empty () in

let b = {keys=[|1;2;3;4;5;6|];
		size=2;
		vals=Sons([|a;a';e;e;e;e|])
		}
in
absorb b 0;
b;;

