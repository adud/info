(*union find*)



let genere a n =
  let f aleph =
    let i = random__int n in
    let j = random__int (n-1) in
    if
      j < i
    then
      (i,j)
    else
      (i,j+1)
  in
  init_vect a f
;;

let rec find r i =
  let scan = if r.(i) = i then i else find r r.(i) in
  r.(i) <- scan;
  scan
 
;;

let calc_taille r taille i = taille.(find r i)
;;

let tmax = ref 0
;;
  
let union r taille i j =
  let cci = find r i in
  let ccj = find r j in
  if
    cci <> ccj
  then (*c'est cci qu'on va lier a ccj*)
    if taille.(cci) < taille.(ccj)
    then
      begin
        r.(cci) <- ccj;
        taille.(ccj) <- taille.(cci) + taille.(ccj);
        tmax := max taille.(ccj) !tmax
      end
    else
      begin
        r.(ccj) <- cci;
        taille.(cci) <- taille.(cci) + taille.(ccj);
        tmax := max taille.(cci) !tmax
      end
;;

let taille_pgcc n t =
  let taille = make_vect n 1 in
  let r = init_vect n (fun i -> i) in
  tmax := 0;
  let f k =
    let i, j = t.(k) in
    union r taille i j;
    !tmax
  in
  init_vect (vect_length t) f
;;

let n = 10000000;;
let a = 100000;;
  
let t = genere a n;; 
taille_pgcc n t;;

