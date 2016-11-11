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
let cartesian = cartesian_aux []
;;

cartesian [1;2;3] [4;5;6]
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




