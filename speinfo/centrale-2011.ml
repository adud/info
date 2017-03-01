type ch = A|B
;;
type mot == ch list
;;

let rec puiss2 n =
  if n = 0 then 1 else 2*puiss2 (n-1)
;;  
let rec comptb m n =
  match
    m
  with
  |[] -> n = 0
  |A::_ -> false
  |B::r -> comptb r (n-1)
;;
  
let rec compta m n =
  match m with
  |[] -> true
  |A::r -> compta r (n+1)
  |B::r -> comptb (B::r) n
;;
  
let rl0 (m:mot) = compta m 0
;;

let rec compta' m n =
  match m with
  |[] -> true
  |A::r -> compta' r (n+1)
  |B::r -> comptb' (B::r) n

and comptb' m n =
  match
    m
  with
  |[] -> n = 0
  |A::r -> n = 0 && compta' (A::r) 0
  |B::r -> comptb' r (n-1)
;;
  
let rl0'' (m:mot) = compta' m 0
;;

let rl1 (m:mot) =
  let rec puiss2 n =
    if
      n mod 2 = 0
    then
      puiss2 (n/2)
    else
      n = 1
  in
  puiss2 (list_length m)
;;


(*PART II*)

(*bits de poids faible en premier*)
  
let base_2 k n =
  let v = ref k in
  let t = make_vect n false in
  for i = 0 to n-1 do
    t.(i) <- !v mod 2 = 1;
    v := !v / 2;
  done;
  t
;;
(* sous_facteurs string -> int -> string list
sous_facteurs m t retourne l'ensemble des sous-facteurs de m
décrits par le sous-ensemble de {0,...,p} décrit par k
avec p = (string_length m - 2)*)
  
let sous_facteurs m k =
  let p = string_length m - 2 in
  let t = base_2 k (p+1) in
  let li = ref [] in
  let beg = ref 0 in
  for i = 0 to p do
    if t.(i) then
      begin
        li := sub_string m !beg (i + 1 - !beg) :: !li;
        beg := i+1;
      end
  done;
  sub_string m !beg (p + 2 - !beg)::!li
;;
  
let dans_L_etoile dans_L m =
  let p = string_length m - 2 in
  let rec existe f k l =
    if
      k > l
    then
      false
    else
      f k || existe f (k+1) l
  in
  let conc_L k =
    let sf = sous_facteurs m k in
    for_all dans_L sf
  in
  existe conc_L 0 (puiss2 (p+1))
;;
      
 
