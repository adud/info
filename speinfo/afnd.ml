(* type des ensembles finis *)
type 'a ensf == 'a list;;
type ('a, 'b) automate = {
  alphabet : 'a ensf;
  etats : 'b ensf;
  initiaux : 'b ensf;
  finaux : 'b ensf;
  transitions : ('b * 'a * 'b) ensf;
}
;;

type 'a mot = 'a list;;
type 'a expr =
  | Zero
  | Un
  | Lettre of 'a
  | Conc of 'a expr * 'a expr
  | Etoile of 'a expr
  | Plus of 'a expr * 'a expr
;;
