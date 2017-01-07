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

let spawn_car per ph v pos sec itin t =
  if
    (t-ph) mod per = 0
  then
    begin
      ajcar_sil sec (creer_voiture v itin) pos
    end
  else
    ()
;;

let jouer p i f =
  for t = i to f do
    afficher p;
    iterer p t;
    print_newline ();
  done;
    afficher p;
;;
