(* TP6 *)

(* TP réalisé par Lucas GUERRY et Rémi GATTAZ *)

(* Exercice 79 *)
#load "dynlink.cma";;
#load "camlp4o.cma";;

let digit c = int_of_char c - int_of_char '0';;

let rec horner n = parser
  | [< ''0'..'9' as c ; m = horner (10 * n + digit c) >] -> m
  | [< >] -> n;;


(* Exercice 80 *)
type pion = Noir | Blanc;;
type coup = Coup of pion * int * int;;

type plateau = Plateau of int * int * coup list;;

(* Exercice 81 *)
let rec mange_espace = parser
	       | [< '' '; s >] -> mange_espace s
	       | [< >] -> ();;

let parser_pion = parser
  | [< ''b'; ''l'; ''a'; ''n'; ''c';>] -> Blanc
  | [< ''n'; ''o'; ''i'; ''r';>] -> Noir
  | [< 'x >]-> failwith "Fail to read color";;

let parser_coup = parser
	       | [< ''('; ()=mange_espace; m=(parser_pion); ()=mange_espace; x = (horner 0); ()=mange_espace; y = (horner 0); ()=mange_espace; '')' >] -> (Coup (m,x,y))
	       | [< 'x >]-> failwith "Fail to read move";;

let rec parser_coupList = parser
	       | [< c = parser_coup; ()=mange_espace;l=parser_coupList >] -> (c::l)
	       | [< 'x >]-> failwith "Fail to read move list"
	       | [< >] -> [];;

let parser_taillePlateau = parser
	       | [< ''('; ()=mange_espace; x=(horner 0); ()=mange_espace; y=(horner 0); ()=mange_espace; '')'; >] -> (x,y)
	       | [< 'x >] -> failwith "Failed to read board size";;

let parser_sys = parser
	       | [< ()=mange_espace; (x,y)=parser_taillePlateau; ()=mange_espace; l=(parser_coupList) >] -> (Plateau (x,y,l))
	       | [< 'x >]-> failwith "Fail to read game";;

let test_pion1 = Stream.of_string "noir";;
let test_pion2 = Stream.of_string "blanc";;
let test_pion3 = Stream.of_string "bleu";;
let test_pion4 = Stream.of_string "jaune";;
(parser_pion test_pion1);;
(parser_pion test_pion2);;
(parser_pion test_pion3);; (* Comment détecter l'erreur pour ce cas ? *)
(parser_pion test_pion4);;

let test_coup1 = Stream.of_string "(blanc 3 6)";;
let test_coup2 = Stream.of_string "(   blanc   3    6)";;
let test_coup3 = Stream.of_string "blanc 3 6)";;
(parser_coup test_coup1);;
(parser_coup test_coup2);;
(parser_coup test_coup3);;

let test_coupList1 = Stream.of_string "(noir 3 5)(blanc 5 8)(blanc 5 9)(noir 3 4)(noir 3 6)";;
let test_coupList2 = Stream.of_string "(noir 3 5)   (blanc   5 8) (    blanc   5   9  )(   noir 3 4)( noir 3 6  )";;
(parser_coupList test_coupList1);;
(parser_coupList test_coupList2);;

let test_sys1 = Stream.of_string "(19 19)(noir 3 5)(blanc 5 8)(blanc 5 9)(noir 3 4)(noir 3 6)";;
let test_sys2 = Stream.of_string "( 19    19  )  (noir 3 5)   (blanc   5 8) (    blanc   5   9  ) (   noir 3 4) ( noir 3 6  )";;
let partie1 = (parser_sys test_sys1);;
(parser_sys test_sys2);;

let lit_partie = parser_sys;;

(* La grammaire définie permet les espaces n'importe ou tant que la structure des parenthèse est correcte. Cependant, cela rend certaines fonction parser peut claire.*)

(* Exercice 83 *)
let rec append (aList: 'a list) (aElem: 'a):'a list =
  match aList with
  | [] -> [aElem]
  | wElem::wList -> wElem::(append wList aElem);;

let joue_coup (aPlateau : plateau) (aCoup:coup) : plateau =
  let (Plateau(x,y,wList)) = aPlateau in
  Plateau(x,y,(append wList aCoup));;

(joue_coup partie1 (Coup (Blanc,2,2)));;

