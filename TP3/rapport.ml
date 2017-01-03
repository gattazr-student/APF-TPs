(* TP3 *)

(* TP réalisé par Rémi GATTAZ et Quentin MACE*)

(* Exercice 36 *)
let rec est_trie (aList : int list) : bool =
  ( match aList with
      [] -> true
    | [wElem] -> true
    | wElem1::wElem2::wList -> (if wElem1 < wElem2 then (est_trie (wElem2::wList)) else false)
  );;

(* Exercice 37 *)
let rec decroissante (aNbElem : int) : int list =
  ( if aNbElem = 1 then [1] else  (aNbElem::(decroissante (aNbElem-1)))
  );;

(decroissante 15);;

let rec croissante (aNbElem : int) : int list =
  ( if aNbElem = 1 then [1] else (croissante (aNbElem-1)@[aNbElem])
  );;

(croissante 15);;

Random.init(1);;

let rec aleatoire (aNbElem : int) : int list =
 ( let wMax = 99 in
   ( if aNbElem > 0 then Random.int(wMax)::(aleatoire (aNbElem-1))
     else []
   )
 );;

(aleatoire 15);;

(* Exercice 38 *)
let rec trouve_min (aList : int list) : int * int list =
  ( match aList with
      [wElem] -> (wElem , [])
      | wElem::wList -> (let (wMin, wList2) = (trouve_min wList) in
		       (if wElem < wMin then (wElem, wList) else (wMin, wElem::wList2))
    )
  );;

let list1 = (aleatoire  15);;
(trouve_min list1);;

(* Exercice 39 *)
let rec tri_selection (aList : int list) : int list =
  ( match aList with
      [] -> []
    | wElem::wList -> ( let (wMin,wList2) = (trouve_min aList) in
			wMin::(tri_selection wList2)
    )
  );;

(tri_selection list1);;


(* Exercice 42 *)
(* Insère dans un liste trié croissante *)
let rec insere (aList : int list) (aElem : int) =
  ( match aList with
      [] -> [aElem]
    | wElem::wList -> (if aElem < wElem then aElem::aList else wElem::(insere wList aElem))
  );;

let list1 = (aleatoire  3);;
let list1= (tri_selection list1);;
(insere list1 89);;

(* Exercice 43 *)
let rec tri_insertion (aList : int list) : int list =
  ( match aList with
      [] -> []
    | wElem::wList -> (insere (tri_insertion wList) wElem)
  );;

let list2 = (aleatoire  5);;
(tri_insertion list2);;

(* Exercice 44 *)
let rec decoupe (aList : int list) (aPivot : int): int list * int list =
  ( match aList with
      [] -> ([], [])
    | wElem::wList -> (let (wList1, wList2) = (decoupe wList aPivot) in
		       ( if wElem <= aPivot then ((wElem::wList1), wList2)
			 else (wList1, (wElem::wList2))
		       )
    )
  );;

(* Exercice 45 *)
let rec tri_rapide (aList : int list) : int list =
  ( match aList with
      [] -> []
    | wElem::wList -> ( let (wP1, wP2) = (decoupe wList wElem) in
			(tri_rapide wP1)@[wElem]@(tri_rapide wP2)
    )
  );;

let alea3 = (aleatoire 15);;
(decoupe alea3 50);;
(tri_rapide alea3);;
