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
  if r.(i) = i then i else find r r.(i)
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
    begin
      r.(cci) <- ccj;
      taille.(ccj) <- taille.(cci) + taille.(ccj);
      tmax := max taille.(ccj) !tmax
    end
;;

let taille_pgcc n t =
  let taille = make_vect n 1 in
  let r = init_vect n (fun i -> i) in
  tmax := 1;
  let f k =
    let i, j = t.(k) in
    union r taille i j;
    !tmax
  in
  init_vect (n-1) f
;;

let n = 10;;
  
let t = genere 10 n
;;

  taille_pgcc n t;;
