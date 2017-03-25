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

(*existe : (int -> bool) -> int -> int 
existe f k l renvoie true ssi il existe n dans [|k;l|] tq
f n soit vrai*)
let rec existe f k l =
    if
      k > l
    then
      false
    else
      f k || existe f (k+1) l
;;
  
let dans_L_etoile dans_L m =
  let p = string_length m - 2 in
  let conc_L k =
    let sf = sous_facteurs m k in
    for_all dans_L sf
  in
  existe conc_L 0 (puiss2 (p+1))
;;

(*decoupe : string -> int -> string * string :
decoupe k m retourne le couple m[:k] et m[k:]*)  

let decoupe m k =
  let n = string_length m in
  sub_string m 0 k, sub_string m k (n-k)
;;  
let rec dans_L_etoile2 dans_L m =
  let n = string_length m in
  let bonne_decoupe k =
    let deb,fin = decoupe m k in
    dans_L deb && dans_L_etoile2 dans_L fin
  in
  dans_L m || existe bonne_decoupe 1 (n-1)
;;

let dans_L_etoile3 dans_L m =
  let n = string_length m in
  let t = make_matrix n n false in
  for j = 0 to n-1 do
    for i = j downto 0 do
      t.(i).(j) <-
        dans_L (sub_string m i (j-i+1)) ||
          existe (fun k -> t.(i).(k) &&t.(k+1).(j)) i (j-1)
    done;
  done;
  t.(0).(n-1)
;;

let dsaaoub m = (m = "aa" || m = "b")
;;

  
  (*EN FILE*)
type fifo = {contenu:int vect;
             mutable debut:int;
             mutable fin:int}
;;

let creer_file n = {contenu = make_vect n 0;debut=0;fin=(-1)}
;;

let est_vide f = f.fin = f.debut - 1
;;

let put a f =
  let n = vect_length f.contenu in
  
  begin
    f.fin  <- (f.fin + 1) mod n;
    f.contenu.(f.fin) <- a;
  end
;;

let get f =
  let n = vect_length f.contenu in
  if f.debut = f.fin + 1
  then failwith "file vide"
  else
    begin
      let a = f.contenu.(f.debut) in
      f.debut <- (f.debut + 1) mod n;
      a
    end
;;
                        
let dans_L_etoile4 dans_L m =
  let n = string_length m in
  let vus = make_vect n false in
  let q = creer_file (n+1) in
  put (-1) q;
  while not est_vide q && not vus.(n-1) do
    let s = get q in
    for i = s + 1 to (n-1) do
      if not vus.(i) && dans_L (sub_string m (s+1) (i-s))
      then
        begin
          vus.(i) <- true;
          put i q;
        end
    done
  done;
  vus.(n-1)
;;

dans_L_etoile3 dsaaoub "aaaaaabbbbbbaa";;
