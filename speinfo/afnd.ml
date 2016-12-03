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

type 'a mot = 'a list;;
type 'a expr =
  | Zero
  | Un
  | Lettre of 'a
  | Conc of 'a expr * 'a expr
  | Etoile of 'a expr
  | Plus of 'a expr * 'a expr
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
    execute aut nets mot
;;

let est_reconnu aut mot =
  [] <> inter aut.finaux (execute aut aut.initiaux mot)
;;

