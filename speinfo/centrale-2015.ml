let conflit i j = 
	let ib,ie = i in
	let jb,je = j in
	ie > jb && je > ib (*not (max i < min j) || (max j < min i)*)
;;

let construit_graphe lin =
	let n = vect_length lin in
	let a = make_vect n [] in
	for k=0 to n-1 do
		for l=n-1 downto 0 do
			if k<>l && conflit lin.(k) lin.(l)
			then a.(k) <- l::a.(k)
			else ()
		done
	done;
	a
;;

construit_graphe [|(0,3);(1,3);(2,5);(4,7);(6,10);(8,9);(11,12)|]
;;

let rec appartient l x =
	match l with
	|[] -> false
	|y::r -> x=y || appartient r x
;;

let plus_petit_absent l =
	let r = ref 0 in
	while appartient l !r do
		incr r
	done;
	!r
;;

plus_petit_absent [0;1;2;3;4];;

let iter_succ f ar n =
	do_list f ar.(n)
;;

let couleurs_voisins aretes couleurs i =
	let cvois = ref [] in
	let ajout_coul v =
		let c = couleurs.(v) in
		if c <> (-1) && not (appartient !cvois c)
		then cvois := c::!cvois
	in
	iter_succ ajout_coul aretes i;
	!cvois
;;

let couleur_disponible aretes couleurs i =
	plus_petit_absent (couleurs_voisins aretes couleurs i)
;;


let est_clique aretes xs =
	for_all (fun x -> for_all (fun y ->  appartient aretes.(x) y ) xs) xs
;;

let coloration segments aretes = 
	let n = vect_length segments in
	let couleurs = make_vect n (-1) in
	for i=0 to n-1 do
		couleurs.(i) <- couleur_disponible aretes couleurs i
	done;
	couleurs
;;

