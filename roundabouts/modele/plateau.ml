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

type rond_point =
  {ents: section list; sors : section list; rond: section list;
   diss: distr list}
    
let creer_rond_point n l p vmi vme cmp =
  
  let sp = creer_spawn () in
  let dumi = creer_inter 0 passif in
  let isec = Array.make n dumi in
  
  let dums = creer_section 0 0 in
  let ent = Array.make n dums in
  let sor = Array.make n dums in
  let ron = Array.make n dums in

  (*creation des sections et des intersections*)
  for i=0 to (n-1) do
    ent.(i) <- creer_section l vme;
    sor.(i) <- creer_section l vme;
    ron.(i) <- creer_section p vmi;
    isec.(i) <- creer_inter 2 cmp;    
  done;

(*liaisons internes*)
  for i=0 to n-2 do
    lier isec.(i) 0 isec.(i+1) 0 ron.(i)
  done;
  lier isec.(n-1) 0 isec.(0) 0 ron.(n-1);

(*liaisons externes*)
  for i=0 to n-1 do
    lier isec.(i) 0 (creer_sortie "") 1 sor.(i) ;
    lier sp 1 isec.(i) 1 ent.(i);
    ajouter_sortie isec.(i) 1 ron.(i)
  done;

    {ents=Array.to_list ent;
     sors=Array.to_list sor;
     rond=Array.to_list ron;
     diss=Array.to_list isec}
;;

let pl_add_rp pl rp =
  pl.sects <- rp.ents @ rp.sors @ rp.rond @ pl.sects;
  pl.distrs <- rp.diss @ pl.distrs;
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
  

