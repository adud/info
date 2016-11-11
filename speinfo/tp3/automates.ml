type automate = {
  etats : int list ; (* ensemble des états *)
  q0 : int; (* état initial *)
  finaux : int list; (* liste des états finaux *)
  (* la fonction de transition est représentée par la liste des
  triplets (q, a, q') tels que delta(q, a) = q' : *)
  transitions : (int * char * int) list;
};;
exception Blocage;;

(*assoc3 : ('a*'b*'c) list -> 'a -> 'b -> 'c
assoc3 li a b retourne c' du premier triplet 
(a',b',c') de li tq a'=a et b'=b et soulève
Blocage sinon*)


let rec assoc3 li q c = match li with
  |[] -> raise Blocage
  |tr::r -> let q1,c1,q2 = tr in
	    if q1 = q && c1 = c then q2
	    else assoc3 r q c
;;  
(*execute_aux int -> automate -> char list -> int
Si l'automate aux se trouve dans l'état q et qu'on
lui fait passer mot,
execute_aux q aut mot retourne l'état de l'automate
après que le mot ait été passé si l'automate ne
bloque pas, soulève Blocage sinon *)
  
let rec execute_aux aut mot q = match mot with
  |[] -> q
  |c::r -> let q2 = assoc3 aut.transitions q c in
	   execute_aux aut r q
;;

let execute aut mot = execute_aux aut mot aut.q0
;;

let reconnait aut mot =
  try
    let final = execute aut mot in
    mem final aut.finaux
  with
  |Blocage -> false
;;

let recon_a = {etats=[0];q0=0;finaux=[0];transitions=[(0,`a`,0)]}
;;
let recon_mot_vide = {etats=[0];q0=0;finaux=[0];transitions=[]}
;;
let recon_rien = {etats=[0];q0=0;finaux=[];transitions=[]}
;;
let test = []
;;
  reconnait recon_a test
;;
  reconnait recon_mot_vide test
;;
  reconnait recon_rien test
;;

(*nbp : int -> char -> bool
   nbp q c renvoie true si l'automate dans l'état q ne
   bloque pas à la lecture de la lettre c et false sinon*)

let nbp aut q c =
  exists (fun (q1,c1,q2) -> q1=q && c1=c) aut.transitions
;;
    
  
let est_complet alph aut =
  (*ecp : int -> bool
   ecp q renvoie true si l'automate dans l'état q ne bloque jamais
   à la lecture d'une lettre de alph et false sinon*)
  let ecp q =
    for_all (fun c -> nbp aut q c) alph in
  for_all (fun q -> ecp q) aut.etats
;;

  (*maxlist : 'a list -> 'a
maxlist li retourne le maximum de la liste li non-vide*)
let maxlist li = it_list max (hd li) li
;;
  maxlist [0;1;2;3;4]
;;
(*accoupler : 'a -> 'b list -> ('a * 'b) list -> ('a * 'b) list
accoupler s a [b1;...;bn] retourne [(a,b1);...;(a,bn)]@s *)

let rec accoupler s a l = match l with
	|[] -> s
	|b::r -> (a,b)::accoupler s a r 
;;

(*cartesian_aux:('a * 'b) list -> 'a list -> 'b list -> ('a * 'b) list
cartesian_aux s l1 l2 retourne (l1 x l2)@s *)

let rec cartesian_aux s l1 l2 = match l1,l2 with
	|[],_|_,[] -> s
	|a::q,l -> let s2 = cartesian_aux s q l in
		accoupler s2 a l 
;;
(* cartesian : '_a list -> '_b list -> ('_a * '_b) list 
cartesian l1 l2 retourne l1 x l2 *)
let cartesian a b = cartesian_aux [] a b
;;

cartesian [1;2;3] [4;5;6]
;;
cartesian
;;

(*
etendre:('a * 'b * 'c) list -> 'a -> 'b * 'c -> 'a * 'b * 'c
etendre trans puits (q,c) retourne (q,c,q') si (q,c,q') est dans trans
ou (q,c,puits) sinon
i.e. si q.c est d�fini dans les transitions trans, alors retourne la transition q -(c)-> q' 
sinon, retourne la transition q -(c)-> puits*)

let etendre trans puits (q,c) =
	try
		let q' = assoc3 trans q c in
		(q,c,q')
	with
	|Blocage -> (q,c,puits)
;;
	
let complete alph aut =
  if est_complet alph aut then aut
  else
    let puits = maxlist aut.etats + 1 in
    let Q = puits::aut.etats in
    let ens_dep = cartesian Q alph in
    let trans = map (etendre aut.transitions puits) ens_dep
    in {etats=Q;q0=aut.q0;finaux=aut.finaux;transitions=trans}
;;

let alph = [`a`;`b`;`c`;`d`] in
est_complet alph (complete alph recon_a)
;;

(* bijection grace a la division euclidienne *)

let code p q (i,j) = q*i+j
;;
let decode p q n = n/q,n mod q
;;

let p,q=3,4 in
decode p q (code p q (3,0));;


(*filter ('a -> bool) -> 'a list -> 'a list
filter f l retourne la liste des �l�ments de l v�rifiant f*)
let rec filter f l = match l with
	|[]->[]
	|r::q -> if f r 
			 then r::filter f q
			 else filter f q
;;

let produit aut1 aut2 =
	let Q1,Q2 = aut1.etats,aut2.etats in
	let codage = code (list_length Q1) (list_length Q2) in
	let codlist = map codage in
	let Q = codlist (cartesian Q1 Q2) in
	let initiaux = codage (aut1.q0, aut2.q0) in
	let F = codlist (cartesian aut1.finaux aut2.finaux) in
	
	(*le plus d�licat : faire le produit des fonctions de transition
	ici : faire violament un produit cart�sien puis choisir seulement
	les couples d'�tats ayant la m�me lettre*)
	let trans1,trans2 = aut1.transitions,aut2.transitions in
	let fi ((q1,a1,q1'),(q2,a2,q2')) = a1 = a2 in
	let filtrat = filter fi (cartesian trans1 trans2) in
	(*purification du filtrat : codage de chacune des valeurs*)
	let codtrans ((q1,a1,q1'),(q2,a2,q2')) =
		(codage (q1,q2),a1,codage (q1',q2')) in
	let delta = map codtrans filtrat in
	(*l'automate, enfin*)
	{etats=Q;q0=initiaux;finaux=F;transitions=delta}
;;



