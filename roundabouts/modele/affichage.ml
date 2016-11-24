open Objets
;;

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

let draw_section sec =
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

let print_car c =
  print_int (radar c)


let print_inter dis =
  let affd (v,d,e,s) =
    print_int (radar v);
    print_string " ";
    print_int d;
  in
  let a = patients dis in
  List.iter affd a;
  print_newline ();
;;
