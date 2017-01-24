open Objets
;;
open Affichage
;;
open Comportements
;;

type plateau =
  {mutable sects:section list;
   mutable distrs:distr list;
   mutable spawns:
             (int -> unit) list
  }
;;

let construire s d sp = {sects=s;distrs=d;spawns=sp};;
  
let iterer p t =
  List.iter (fun f -> f t) p.spawns;
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
    prob < r
  then
    ajcar_sil sec (creer_voiture v itin "truc") pos
  ;
    ignore t
;;

let faire p i f dbt bent bsor fin =
  dbt p;
  for t = i to (f-1) do
    bent p;
    iterer p t;
    bsor p;
  done;
  fin p;
;;

let rien (p:plateau) = ();;
  


let jouer p i f =
  let aff p =
    afficher p;
    print_newline ()
  in
  faire p i f rien aff rien aff
;;

let animer p i f grcr =
  let bent p =
    Graphics.clear_graph ();
    imager p grcr;
    Graphics.synchronize ();
    ignore (Graphics.wait_next_event [Graphics.Key_pressed]);  
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
  

