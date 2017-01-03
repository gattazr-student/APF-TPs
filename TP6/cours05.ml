print_int;;
print_int 3;;
let _ = print_int 3 in print_int 5;;
let _ = print_int 8 in false;;


(* Analyse descendante récursive sur une liste *)
(*
 S ::= (S) | x
*)

exception Echec
type analist = char list -> char list

(* Consommation d'un caractère précis en début de mot *)
let p_parouv: analist = fun l -> match l with
  | '(' :: l1 -> l1
  | _ -> raise Echec

let p_parfer: analist = fun l -> match l with
  | ')' :: l1 -> l1
  | _ -> raise Echec

let p_x: analist = fun l -> match l with
  | 'x' :: l1 -> l1
  | _ -> raise Echec

(* Consommation de S en début de mot *)
(* La fonction de consommation de S est passée en paramètre *)
let p_ouvfer: analist -> analist = fun p_S l ->
  let l1 = p_parouv l in
  let l2 = p_S l1 in
  let l3 = p_parfer l2 in
  l3

let rec p_S: analist = fun l ->
  try p_ouvfer p_S l with
  | Echec -> p_x l


let list_of_string s = 
  let rec boucle s i n = 
    if i = n then [] else s.[i] :: boucle s (i+1) n
  in boucle s 0 (String.length s)

let ch1 = list_of_string "((x))()abc"
let _ = p_S ch1
let ch1 = list_of_string "((y))()"
let _ = p_S ch1
let ch1 = list_of_string "((x)x)"
let _ = p_S ch1


(* Flots *)


#load "dynlink.cma"
#load "camlp4o.cma"

let flux_int_19 =[<'1; '9>]
let flux_char_ab =[<''a'; ''b'>]
let flux_char_cd =[<''c'; ''d'>]
let flux_char_ab_cd = [< flux_char_ab; flux_char_cd>]
let flux_char_1934 = Stream.of_string "1934"

let f1 = [< '19 ; '345 >]
let f1 = [< '"19" ; '"34" >]

let rec nat_list n = n :: nat_list (n+1)
let nat = nat_list 0

let rec nat_stream n = [< 'n ; nat_stream (n+1) >] 
let nat = nat_stream 0 

let next = parser
  | [<'x >] -> x

let _ = next flux_char_ab
let _ = next flux_char_ab
let _ = next flux_char_ab
let _ = next flux_char_ab_cd
let _ = next flux_char_ab_cd
let _ = next flux_char_ab_cd
let _ = next flux_char_ab_cd
let _ = next flux_char_ab_cd
let _ = next flux_char_cd

let _ = next nat
let _ = next nat
let _ = next nat
let _ = next nat
let _ = next nat





let consomme_aveuglement = parser 
  [< '_ >] -> 0

(* Grammaires *)

let rec parent = parser
  | [< ''('; n = parent; '')'  >] -> n+1
  | [< ''x' >] -> 0

let s1 = Stream.of_string "(((x)))"
let s2 = Stream.of_string "((()))"
let s3 = Stream.of_string "((x))()"

let _ = parent s1
let _ = parent s2
let _ = parent s3

let _ = next s1;;
let _ = next s2;;
let _ = next s3;;



(* Lecture d'un entier *)

let digit c = int_of_char c - int_of_char '0';;

let rec horner n = parser
  | [< ''0'..'9' as c ; s >] -> horner (10 * n + digit c) s
  | [< >] -> n;;

let rec horner n = parser
  | [< ''0'..'9' as c ; m = horner (10 * n + digit c) >] -> m
  | [< >] -> n;;

let s = Stream.of_string "132+422";;

horner 0 s;;

consomme_aveuglement s;;

horner 0 s;;

let p_E = parser
  | [< t1 = horner 0 ; ''+' ; t2 = horner 0 >] -> t1 + t2
  | [< t = horner 0 >] -> t ;;

let s = Stream.of_string "132+422";;
p_E s;;
let s = Stream.of_string "132422";;
p_E s;;
let s = Stream.of_string "132+422+1";;
p_E s;;


let p_SE = parser
  | [< ''+' ; t = horner 0 >] -> (fun x -> x+t)
  | [< >] -> (fun x -> x) ;;

let p_E2 = parser
  | [< t = horner 0 ; se = p_SE >] -> se t;;

let s = Stream.of_string "132+422";;
p_E2 s;;
let s = Stream.of_string "132422";;
p_E2 s;;
let s = Stream.of_string "132+422+1";;
p_E2 s;;

