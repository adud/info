let print_section (sec:Objets.section) = 
  let d = sec.data in
  for i = 0 to (Array.length d) - 1 do
    match
      d.(i)
    with
    |None -> ()
    |Some(c) -> print_int c.spd
  done
;;

