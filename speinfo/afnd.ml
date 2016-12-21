(* type des ensembles finis *)
type 'a ensf == 'a list;;
type ('a, 'b) automate = {
  alphabet : 'a ensf;
  etats : 'b ensf;
  initiaux : 'b ensf;
  finaux : 'b ensf;
  transitions : ('b * 'a * 'b) ensf;
}
;;

type 'a mot == 'a list;;
type 'a expr =
  | Zero
  | Un
  | Lettre of 'a
  | Conc of 'a expr * 'a expr
  | Etoile of 'a expr
  | Plus of 'a expr * 'a expr
;;

let rec appartient x ens =
  match
    ens
  with
  |[] -> false
  |a::r -> a=x || appartient x r
;;

let rec inter li1 li2 =
  match
    li1,li2
  with
  |[],_|_,[] -> []
  |x::r,y::s -> 
    if
      x<y
    then
      inter r li2
    else
      if
	x=y
      then
	x::inter r s
      else (*y<x*)
	inter li1 s
	
;;

let ex1,ex2 = [1;2;3;5], [2;5;6]
;;

let rec union li1 li2 =
  match
    li1,li2
  with
  |[],_ -> li2
  |_,[] -> li1
  |x::r,y::s -> 
    if
      x < y
    then
      x::union r li2
    else
      if
	x=y
      then
	x::union r s
   
      else (*x > y*)
	y::union li1 s
;;
	
union ex1 ex2
;;

let rec grouper lli =
  match
    lli
  with
  |[]|_::[] -> lli
  |x::y::r -> (union x y)::grouper r
;;

let rec union_liste lli = 
  match
    lli
  with
  |[] -> []
  |[x] -> x
  |ll' -> union_liste (grouper ll')
;;
union_liste [[1];[2];[3];[4]];;

let union_fun lli f =
  union_liste (map f lli)
;;

let ensemble li = 
  union_fun li (fun x -> [x])
;;

ensemble [1;6;4;7;5;3;6;7;9;9];;

let rec ajoute x li =
  match
    li
  with
  |[] -> [x]
  |y::r ->
    if
      x<y
    then
      x::li
    else
      if
	x=y
      then
	li
      else (*x>y*)
	y::ajoute x r
;;
	
let rec parties ens =
  match
    ens
  with
  |[] -> [[]] (*singleton ens vide*)
  |x::r -> let p = parties r in
	   union p (map (fun e -> ajoute x e) p)
;;
parties ex1;;

(*filext : 'a list -> ('a -> bool) -> ('a -> 'b) -> 'b list
filext ens f g filtre les elements de ens verifiant f
et leur applique la transformation g*)

let rec filext ens f g =
  match
    ens
  with
  |[] -> []
  |x::r -> 
    if
      f x
    then
      (g x)::filext r f g
    else
      filext r f g
;;

let filter ens f = filext ens f (fun x -> x)
;;
let execute_lettre aut et ch =
  let trois (a,b,c) = c in
  let dxidem a' b' (a,b,c) = (a=a') & (b=b') in
  filext aut.transitions (dxidem et ch) trois
;;

let rec execute aut ets mot =
  match
    mot
  with
  |[] -> ets
  |ch::r -> 
    let nets = union_fun ets (fun et -> execute_lettre aut et ch) in
    execute aut nets r
;;

let nondisj a b = [] <> inter a b
;;

let est_reconnu aut mot =
  nondisj aut.finaux (execute aut aut.initiaux mot)
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


let determinise aut =
  let alph = aut.alphabet in
  let ets = parties aut.etats in
  let init = [aut.initiaux] in
  let fin = filter ets (nondisj aut.finaux) in
  let transf (qe,c) = (qe,c,execute aut qe [c]) in
  let trans = map transf (cartesian ets aut.alphabet) in
  {alphabet=alph;
   etats=ets;
   initiaux=init;
   finaux=fin;
   transitions=trans;
  }
;;

let recon_beta = 
  {alphabet=[`a`;`b`];
   etats = ["wait";"ok"];
   initiaux = ["wait"];
   finaux = ["ok"];
   transitions = [("wait",`a`,"ok");("wait",`b`,"wait")];
  }
;;
est_reconnu recon_beta [`b`;`b`;`a`]
;;
determinise recon_beta
;;
(*alph_aux 'a expr -> 'a set -> 'a set
alph_aux exp r retourne (alphabet exp) @ r*)

let rec alph_aux exp r =
  match
    exp
  with
  |Zero|Un -> []
  |Lettre(le) -> le::r
  |Conc(e1,e2) -> alph_aux e1 (alph_aux e2 r)
  |Etoile(ex) -> alph_aux ex r
  |Plus(e1,e2) -> alph_aux e1 (alph_aux e2 r)
;;

let alphabet exp = ensemble (alph_aux exp [])
;;

let beta = Conc (Etoile(Lettre `b`),(Lettre `a`))
;;
let rec contient_epsilon exp =
  match
    exp
  with
  |Zero|Lettre(_) -> false
  |Un|Etoile(_) -> true
  |Conc(e1,e2) -> contient_epsilon e1 && contient_epsilon e2
  |Plus(e1,e2) -> contient_epsilon e1 || contient_epsilon e2
;;

let rec pref1_aux exp r = 
  match exp with
  |Zero|Un -> []
  |Lettre(le) -> le::r
  |Etoile(e) -> pref1_aux e r
  |Plus(e1,e2) -> pref1_aux e1 (pref1_aux e2 r)
  |Conc(e1,e2) -> 
    if
      contient_epsilon e1
    then
      pref1_aux e1 (pref1_aux e2 r)
    else
      pref1_aux e1 r
;;
let pref1 exp = ensemble (pref1_aux exp [])
;;

let rec suf1_aux exp r =
  match
    exp
  with
  |Zero|Un -> r
  |Lettre(le) -> le::r
  |Etoile(e) -> suf1_aux e r
  |Plus(e1,e2) -> suf1_aux e1 (suf1_aux e2 r)
  |Conc(e1,e2) ->
    if
      contient_epsilon e2
    then
      suf1_aux e1 (suf1_aux e2 r)
    else
      suf1_aux e2 r
;;

let suf1 exp = ensemble (suf1_aux exp [])
;;

let rec succ_aux exp le r =
  match
    exp
  with
  |Zero|Un|Lettre(_) -> r
  |Plus(e1,e2) -> succ_aux e1 le (succ_aux e2 le r)
  |Conc(e1,e2) -> 
    let sa = succ_aux e1 le (succ_aux e2 le r) in
    if 
      appartient le (suf1 e1)
    then
     (pref1 e2)@sa
    else sa
  |Etoile(e) ->
    let sa = succ_aux e le r in
    if
      appartient le (suf1 e)
    then
      (pref1 e)@sa
    else
      sa
;;

let succ exp le = ensemble (succ_aux exp le [])
;;

let aut_local exp = 
  let alph = alphabet exp in
  let etats = ajoute None (ensemble (map (fun x -> Some(x)) alph)) in
  let init = [None] in
  let f = ensemble (map (fun x -> Some x) (suf1 exp)) in
  let fin = 
    if
      contient_epsilon exp
    then
      ajoute None f
    else
      f
  in
  let treps = ensemble (map (fun a -> (None,a,Some a)) (pref1 exp)) in
  (*trsuc le toutes les transitions de l'automate local partant de Q_a *)
  let trsuc a = 
    ensemble (map (fun b -> (Some a, b, Some b)) (succ exp a)) in
  let trtr = union_fun alph trsuc (*ttes les transitions sauf celles d'eps*) in
  let trans = union trtr treps in
  {alphabet=alph;
   etats = etats;
   initiaux = init;
   finaux = fin;
   transitions = trans;
  }
;;

let rec lin_aux exp c = 
  match
    exp
  with
  |Zero -> Zero,c
  |Un -> Un,c
  |Lettre(k) -> (Lettre (k,c)),(c+1) 
  |Conc(e1,e2) ->
     let e'1,c1 = lin_aux e1 c in
     let e'2,c2 = lin_aux e2 c1 in
     Conc(e'1,e'2), c2
  |Etoile(e) ->
     let e',c' = lin_aux e c in
     Etoile(e'),c'
  |Plus(e1,e2) ->
     let e'1,c1 = lin_aux e1 c in
     let e'2,c2 = lin_aux e2 c1 in
     Plus(e'1,e'2), c2
;;

let linearise exp = 
  let exp',_ = lin_aux exp 0 in
  exp'
;;

linearise (Conc (Etoile(Lettre(`a`)),Lettre(`a`)));;

let applique_morphisme aut f =
  let alph = ensemble (map f aut.alphabet) in
  let etats = aut.etats in
  let init = aut.initiaux in
  let fin = aut.finaux in
  let trans = ensemble (map (fun (q1,c,q2) -> (q1,f c,q2)) aut.transitions) in
  {alphabet=alph;
   etats=etats;
   initiaux=init;
   finaux=fin;
   transitions=trans;}
;;

let glushkov exp = 
  let explin = linearise exp in
  let autloc = aut_local explin in
  applique_morphisme autloc (fun (c,v) -> c)
;;

let afd exp = determinise (glushkov exp)
;;


