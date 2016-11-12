type automate = {
  etats : int list ; (* ensemble des Žtats *)
  q0 : int; (* Žtat initial *)
  finaux : int list; (* liste des Žtats finaux *)
  (* la fonction de transition est reprŽsentŽe par la liste des
  triplets (q, a, q') tels que delta(q, a) = q' : *)
  transitions : (int * char * int) list;
};;
exception Blocage;;


(*Avant toute chose, je souhaiterais signaler que l'usage d'ensembles
(set) me semble beaucoup plus appropriŽ que les listes, et que je
perds une complexitŽ folle ˆ chaque fois qu'il faut Žviter des doublons*)



(*assoc3 : ('a*'b*'c) list -> 'a -> 'b -> 'c
assoc3 li a b retourne c' du premier triplet 
(a',b',c') de li tq a'=a et b'=b et soulÃ¨ve
Blocage sinon*)


let rec assoc3 li q c = match li with
  |[] -> raise Blocage
  |tr::r -> let q1,c1,q2 = tr in
	    if q1 = q && c1 = c then q2
	    else assoc3 r q c
;;  
(*execute_aux int -> automate -> char list -> int
Si l'automate aux se trouve dans l'Žtat q et qu'on
lui fait passer mot,
execute_aux q aut mot retourne l'Žtat de l'automate
aprÃ¨s que le mot ait ŽtŽ passŽ si l'automate ne
bloque pas, soulÃ¨ve Blocage sinon *)
  
let rec execute_aux aut mot q = match mot with
  |[] -> q
  |c::r -> let q2 = assoc3 aut.transitions q c in
	   execute_aux aut r q2
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
   nbp q c renvoie true si l'automate dans l'Žtat q ne
   bloque pas Ã  la lecture de la lettre c et false sinon*)

let nbp aut q c =
  exists (fun (q1,c1,q2) -> q1=q && c1=c) aut.transitions
;;
    
  
let est_complet alph aut =
  (*ecp : int -> bool
   ecp q renvoie true si l'automate dans l'Žtat q ne bloque jamais
   Ã  la lecture d'une lettre de alph et false sinon*)
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
i.e. si q.c est dŽfini dans les transitions trans, alors retourne la transition q -(c)-> q' 
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
filter f l retourne la liste des ŽlŽments de l vŽrifiant f*)
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
	
	(*le plus dŽlicat : faire le produit des fonctions de transition
	ici : faire violemment un produit cartŽsien puis choisir seulement
	les couples d'Žtats ayant la mme lettre*)
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


(*acces_aux : ('a * 'b * 'c) list -> 'c list -> 'c list
acces_aux [(q1,a,q'1);(q2,b,q'2)...;(qn,k,q'n)] s retourne 
[q'1;...q'n]@s
i.e. acces_aux trans s retourne tous les Žtats ciblŽs par les transitions
de la liste trans, concatŽnŽ ˆ s*)

let rec acces_aux trans s = match trans with
	|[] -> s
	|(q1,a,q2)::r -> let s2 = acces_aux r s in 
		if mem q2 s2 then s2
		else q2::s2
;;

let rec coacces_aux trans s = match trans with
	|[] -> s
	|(q1,a,q2)::r -> let s2 = coacces_aux r s in 
		if not mem q1 s2 then q1::s2
		else s2
;;


(*acces : automate -> int -> int list -> int list
acces aut l1 retourne tous les Žtats accessibles de aut en une Žtape ˆ
partir de l'etat q, le tout concatŽnŽ ˆ s*)

let acces aut q s =
	let trans = filter (fun (q1,a,q2) -> q1=q) aut.transitions in
	acces_aux trans s
;;

let coacces aut q s =
	let trans = filter (fun (q1,a,q2) -> q2=q) aut.transitions in
	coacces_aux trans s
;;

(*accessibles_aux : automate -> int list -> int list -> int list
accessibles_aux aut etats s retourne tous les Žtats de aut accessibles en
une Žtape ˆ partir de etats, le tout concatŽnŽ ˆ s*)

let rec accessibles_aux aut etats s = match etats with
	|[] -> s
	|q::r -> let s2 = accessibles_aux aut r s in
		acces aut q s2
;;

let rec coaccessibles_aux aut etats s = match etats with
	|[] -> s
	|q::r -> let s2 = coaccessibles_aux aut r s in
		coacces aut q s2
;;


(*accessibles_it : automate -> int list -> int list
accessibles_it aut etats retourne tous les Žtats accessibles ˆ partir des
Žtats etats*)

let rec accessibles_it aut etats =
	let etats_it = accessibles_aux aut etats etats in
	if etats_it = etats then etats
	else accessibles_it aut etats_it
;;

let rec coaccessibles_it aut etats =
	let etats_it = coaccessibles_aux aut etats etats in
	if etats_it = etats then etats
	else coaccessibles_it aut etats_it
;;


let accessibles aut = accessibles_it aut [aut.q0]
;;

let coaccessibles aut = coaccessibles_it aut aut.finaux
;;

let acc_test = 
let Q = [1;2;3;4] in
let q = 1 in
let F = [3] in
let trans = [(1,`c`,1);(1,`a`,3);(4,`a`,3);(1,`b`,2);(2,`a`,2);(3,`a`,3)] in
{etats=Q;q0=q;finaux=F;transitions=trans}
;;
accessibles acc_test
;;
coaccessibles acc_test
;;


(*inter_list 'a list -> 'a list -> 'a list
inter l1 l2 retourne une liste contenant les ŽlŽments prŽsents dans
l1 et l2 sans garantie sur la multiplicitŽ *)
let inter_list l1 l2 =
	filter (fun x -> mem x l2) l1
;;

(*emonde : automate -> automate
retourne la version ŽmondŽe d'un automate*)

let emonde aut = 
	let Q = inter_list (accessibles aut) (coaccessibles aut) in
	let F = inter_list Q aut.finaux in
	let delta = 
	filter (fun (q1,a,q2) -> mem q1 Q && mem q2 Q) aut.transitions in
	{etats=Q;q0=aut.q0;finaux=F;transitions=delta}
	
;;

emonde acc_test;;
	

(*parce que la programmation fonctionnelle ca va bien quelques heures
mais la plus, une idŽe fonctionelle est la bienvenue

afd_local char list -> char list -> (char*char) list *)

let afd_local p1 s1 f2 =
	(*crŽe une table associant ˆ chaque caractère une valeur entière
	l'Žtat i sera l'Žtat du caractère c tq (c,i) dans table
	l'automate Žtant local, pour toute transition (q,c,q'),
	q' = i*)
	let table = ref [] in
	let compt = ref 1 in
	let qinit = 0 in
	let delta = ref [] in
	(*assoc_reco*)
	let assoc_reco a =
	 	if mem_assoc a !table
		then assoc a !table
		else begin
			table := (a,!compt)::!table;
			incr compt;
			!compt - 1;
		end
	in
	
	let ajouter_init a =
		let i = assoc_reco a in
		delta := (0,a,i)::!delta
	in
	
	let ajouter_inter (a,b) =
		let i = assoc_reco a in
		let j = assoc_reco b in
		delta := (i,b,j)::!delta
	in
	
	do_list ajouter_init p1;
	do_list ajouter_inter f2;
	
	let F = map (fun x -> assoc_reco x) s1 in
	let Q = map (fun (a,b) -> b) !table in
	
	{etats=0::Q;q0=qinit;finaux=F;transitions=(!delta)}
;;

let child = afd_local [`m`;`p`] [`a`;`n`] [(`m`,`a`);(`a`,`m`);(`a`,`n`);(`p`,`a`);(`a`,`p`)];;

reconnait child [`p`;`a`;`p`;`a`]
;;


