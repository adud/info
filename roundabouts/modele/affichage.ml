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
