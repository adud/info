type car = {mutable spd:int;mutable pos:int};;

type way = car list;;

let vlim = 5;;
let plim = 0.;;

let acc c1 = c1.spd <- min (c1.spd + 1) vlim;;

let desc c1 c2 = c1.spd <- min c1.spd (c2.pos - c1.pos - 1);;

let desc_rand c1 = 
	if Random.float 1. < plim then
		c1.spd <- max 0 (c1.spd - 1);;

let move c1 = c1.pos <- c1.spd + c1.pos ;;


let rec step (w:way) = match w with
	|[] -> ()
	|c1::[] -> 
		acc c1; 
		desc_rand c1;
		move c1;
		
	|c1::c2::rw ->
		acc c1;
		desc c1 c2;
		desc_rand c1;
		move c1;
		step (c2::rw);
;;

let get_pos (w:way) = 
	List.map (fun c -> c.pos) w
;;

let print_way (w:way) =
	let g = get_pos w in
	List.iter (fun x -> print_int x;print_string " ") g;
	print_newline();
;;

let w = [{spd=3;pos=0};{spd=0;pos=2};{spd=0;pos=4}];;

print_newline ();
for i = 0 to 9 do
	step w;
	print_way w;
done
;;

w;;

