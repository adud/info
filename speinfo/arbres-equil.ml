type 'a tree =
  |Empty
  |Node of 'a tree * 'a * 'a tree * int
;;

let height t = match t with
  |Empty -> 0
  |Node(_,_,_,n) -> n
;;

let node l v r = Node(l,v,r,1 + max (height l) (height r))
;;

let rec add v t = match t with
  |Empty -> node Empty v Empty
  |Node(g,x,d,n) ->
    let g',d' =
      if v = x then g,d
      else if v < x then (add v g),d
      else (*v>x*) g,(add v d)
    in
    node g' x d'
;;
  
let rec member v t = 
  match
    t
  with
  |Empty -> false
  |Node(g,x,d,n) ->
    v = x || (v < x && (member v g)) || (v > x && (member v d))
;;
 
let rec min_elt t =
  match
    t
  with
  |Empty -> failwith "min_elt : arbre vide"
  |Node(Empty,x,_,_) -> x
  |Node(g,_,_,_) -> min_elt g (*g <> empty*)
;;

let rec remove_min t =
  match
    t
  with
  |Empty -> failwith "remove_min : arbre vide"
  |Node(Empty,x,d,n) -> d
  |Node(g,x,d,n) ->
    let g' = remove_min g in
    node g' x d
;;

let gen_random_int_tree v n =
  let a = ref Empty in
  for i = 0 to n-1 do
    a := add (random__int v) !a
  done;
  !a
;;

let rec remove v t =
  match t with
  |Empty -> Empty
  |Node(g,x,d,n) ->
    if v < x then node (remove v g) x d
    else if v > x then node g x (remove v d)
    else (*v = x l'element a supprimer*)
      match
	d
      with
      |Empty -> g
      | _ ->
	 let x',d' = min_elt d,remove_min d in
	 node g x' d'
;;

let rotleft t = 
  match
    t
  with
  |Node(g,x,Node(g',y,d',_),_) -> node (node g x g') y d'
  |_ -> failwith "rotleft : pas de rotation possible"
;;
  
let rotright t =
  match
    t
  with
  |Node(Node(g,x,g',_),y,d',_) -> node g x (node g' y d')
  |_ -> failwith "rotright : pas de rotation possible"
;;
  
let rotleftright t =
  match
    t
  with
  |Node(Node(gy,y,Node(gz,z,dz,_),_),x,dx,_) ->
    node (node gy y gz) z (node dz x dx)
  |_ -> failwith "rotleftright : pas de rotation possible"
;;

let rotrightleft t =
  match
    t
  with
  |Node(gx,x,Node(Node(gz,z,dz,_),y,dy,_),_) ->
    node (node gx x gz) z (node dz y dy)
  |_ -> failwith "rotrightleft : pas de rotation possible"
;;
  

let bal l v r =
  let tmp = node l v r in
  if abs (height r - height l) > 2
  then  
    if height r > height l
    then
      match r with
      |Empty -> failwith "ne peut pas arriver"
      |Node(gy,y,dy, _) ->
        if
          height gy <= height dy
        then
          rotleft tmp
        else
          rotrightleft tmp
    else (*height r < height l*)
      match l with
      |Empty ->failwith "ne peut pas arriver"
      |Node(gx,x,gy,_) ->
        if
          height gx >= height gy
        then
          rotright tmp
        else
          rotleftright tmp
  else
    tmp
;;

let rec add_bal v t = match t with
  |Empty -> bal Empty v Empty
  |Node(g,x,d,n) ->
    let g',d' =
      if v = x then g,d
      else if v < x then (add v g),d
      else (*v>x*) g,(add v d)
    in
    bal g' x d'
;;
   

let rec remove_min_bal t =
  match
    t
  with
  |Empty -> failwith "remove_min : arbre vide"
  |Node(Empty,x,d,n) -> d
  |Node(g,x,d,n) ->
    let g' = remove_min g in
    bal g' x d
;;

  
let rec remove_bal v t =
  match t with
  |Empty -> Empty
  |Node(g,x,d,n) ->
    if v < x then bal (remove v g) x d
    else if v > x then bal g x (remove v d)
    else (*v = x l'element a supprimer*)
      match
	d
      with
      |Empty -> g
      | _ ->
	 let x',d' = min_elt d,remove_min d in
	 bal g x' d'
;;

let gen_random_int_tree_bal v n =
  let a = ref Empty in
  for i = 0 to n-1 do
    a := add_bal (random__int v) !a
  done;
  !a
;;
