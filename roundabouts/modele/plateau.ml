open Objets
;;
open Affichage
;;
open Comportements
;;

type plateau =
  {mutable sects:section list;
   mutable distrs:distr list;
   mutable events:
             (int -> unit) list
  }
;;

let construire s d ev = {sects=s;distrs=d;events=ev};;
  
let iterer p t =
  List.iter (fun f -> f t) p.events;
  List.iter increment p.sects;
  List.iter (fun x -> traverser x t) p.distrs;
  
;;
let afficher p =
  List.iter print_section p.sects;
;;

let imager p grcr =
  let f sec =
    try
      let (x,y),th,col = List.assoc sec grcr in
      draw_section sec (x,y) th col
    with
    |Not_found -> ()
  in
  List.iter f p.sects

let spawn_car per ph v pos sec itin t =
  if
    (t-ph) mod per = 0
  then
    ajcar_sil sec (creer_voiture v itin "truc") pos
  else
    ()
;;

let rnd_spawn_car prob v pos sec itin t =
  let r = Random.float 1. in
  if
    r < prob
  then
    ajcar_sil sec (creer_voiture v itin "truc") pos
  ;
    ignore t
;;

let spawn_prog f v pos sec itin t =
  rnd_spawn_car (f t) v pos sec itin t
;;
  

let faire p i f dbt bent bsor fin =
  Random.self_init ();
  dbt p;
  for t = i to (f-1) do
    bent p;
    iterer p t;
    bsor p;
  done;
  fin p;
;;

let rien (p:plateau) = ();;
  
let silence p i f =
  faire p i f rien rien rien rien
;;

let jouer p i f =
  let aff p =
    afficher p;
    print_newline ()
  in
  faire p i f rien aff rien aff
;;

let patience d =
  let tf = (Unix.gettimeofday () )+. d in
  let rec loop () =
    let t = (Unix.gettimeofday ()) in
    if t < tf then loop ()
      (*begin
	print_string "dodo";
	print_newline ();
	Unix.sleep (max 1 (int_of_float (tf -. t)));
	print_string "reveil";
	print_newline ();
	loop ();
      end*)
  in loop ()
;;
  
let animer p i f grcr =
  let bent p =
    Graphics.clear_graph ();
    imager p grcr;
    Graphics.synchronize ();
    patience 0.1;
    (*ignore (Graphics.wait_next_event [Graphics.Key_pressed]);  *)
  in
  faire p i f rien bent rien rien
;;


let modeliser p i f info =
  let bent p =
    info();
    print_newline ();
  in
  faire p i f rien bent rien rien
;;
  

