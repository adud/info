let print_section sec = 
  let d = Objets.observer sec in
  for i = 0 to (Array.length d) - 1 do
    match
      d.(i)
    with
    |None -> print_string "."
    |Some(c) -> print_int (Objets.radar c)
  done;
  print_newline ();
;;

let () =
  print_section Objets.circuit;
  Objets.increment Objets.circuit;
  print_section Objets.circuit;
;;
