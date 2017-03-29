(*etude du manege : theorie des graphes*)
(*un poids pour les give-way*)

let h = 0.01
;;

let creer_manege lext lcen lint =
  for i = 0 to 4 do
    let sor = 5*i in
    let pst = 5*i + 1 in
    let pre = 5*i + 3 in
    let ent = 5*i + 2 in
    let nxt = (5*i + 7) mod 20 in
    let ant = (5*i + 6) mod 20 in
    lier sor pst (lint + h);
    lier pre sor lint;
    lier pst ent lint;
    lier ent pre lint;
    lier pre pst lint;
  done
;;
      
	      
