open Objets

;;  
let print_car c =
  print_int (radar c)

let print_section sec = 
  let d =  observer sec in
  for i = 0 to (Array.length d) - 1 do
    match
      d.(i)
    with
    |None -> print_string "."
    |Some(c) -> print_int ( radar c)
  done;
  print_newline ();
;;
    
let preview_section sec =
  let d = observer sec in
  for i=0 to (Array.length d) - 1 do
    match
      d.(i)
    with
    |None -> print_string " "
    |Some(c) -> print_string "."
  done;
  print_newline ();
;;

let virgule () = print_string ","
;;
    
let info_fdmal lsec =
  let d = densite lsec in
  let v = vitesse_moy lsec in
  List.iter (fun x -> (print_float x;virgule ()))
            [d;v;d*.v]
;;

open Graphics
;;
let foi = float_of_int
;;
let trc = truncate
;;
let pi = 2. *. asin 1.
;;
let scale = 20. (*pix/m sachant dx = 7m *)  
  
let init m n =
  open_graph (" "^string_of_int m^"x"^string_of_int n);
  auto_synchronize false;
;;
(*on travaille uniquement avec des flottants,
la traduction en entiers se fait juste avant
l'appel des fonctions du module Graphics
les valeurs tronquees seront precedees d'un p (comme pixel)
Au fait, on compte aussi en radians*)

let rotation (cx,cy) the (x,y) =
  let th = (-.the) in
  let nx = (x-.cx)*.(cos th) +. (y-.cy)*.(sin th) +. cx in
  let ny = (x-.cx)*.(-1.)*.(sin th) +. (y-.cy)*.(cos th) +. cy in
  (nx,ny)
;;    

  
let case_droite (x,y) =
  let d = (scale /. 2.) in
  [|x-.d,y-.d;x+.d,y-.d;x+.d,y+.d;x-.d,y+.d|]
;;

let case (x,y) th =
  Array.map (rotation (x,y) th) (case_droite (x,y))
;;
    
let sec_contour n (x,y) th =
  let fn = foi n in
  let lex,bex = (x -. scale/.2.),(y -. scale/.2.) in
  let rex,tex = (lex +. fn *. scale),(bex +. scale) in
  let cont = [|lex,bex;
                  lex,tex;
                  rex,tex;
                  rex,bex;
                |] in
  let barre i =
    let absc = lex +. (foi i +.1.) *. scale in
    (absc,bex,absc,tex)
  in
  let dec = Array.init (n-1) barre in

  let contour = Array.map (rotation (x,y) th) cont in
  let f (xa,ya,x',y') =
    let ((xr,yr),(xr',yr')) = rotation (x,y) th (xa,ya),
                              rotation (x,y) th (x',y')
    in
    (xr,yr,xr',yr')
  in
  let decoupe = Array.map f dec in
  contour, decoupe
;;

let sec_voit arr (x,y) th =
  let r = ref [] in
  let f i ex =
    if
      ex
    then
      r := rotation (x,y) th ((x +. (scale *. (foi i))),y)::!r
    else ()
  in
  Array.iteri f arr;
  !r
;;

let draw_voits vsec =
  let f (x,y) =
    fill_circle (trc x) (trc y) (trc (scale *. 0.4))
  in
  List.iter f vsec
;;
  
let draw_contour (cont,dec) =
  let tr (x,y) = (trc x, trc y) in
  let tr2 (x,y,x',y') = (trc x, trc y, trc x', trc y') in
  draw_poly (Array.map tr cont);
  draw_segments (Array.map tr2 dec);
;;

let draw_section sec (px,py) th col =
  let x,y = foi px,foi py in
  let n = tsec sec in
  set_color col;
  draw_contour (sec_contour n (x,y) th);
  draw_voits (sec_voit (survoler sec) (x,y) th);
;;
