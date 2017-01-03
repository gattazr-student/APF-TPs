(* TP4 *)

(* TP réalisé par Elsa NAVARRO et Rémi GATTAZ *)

Random.init(1);;
let rec aleatoire (aNbElem : int) : int list = 
 (let wMax = 99 in
   (if aNbElem > 0 then Random.int(wMax)::(aleatoire (aNbElem-1))
     else []
   )
 );;

(* Exercice 50 *)
let rec neme (aList : int list) (aIndex : int): int = 
  (if aIndex < 1 then failwith "index must be > 1" else
      (match aList with
	  [] -> failwith "index not defined"
	| wElem::wList -> 
	  (if aIndex = 1 then wElem else (neme wList (aIndex-1) ) )
	
      )
  );;

(neme [1;2;3;4;5] 1);;
(neme [1;2;3;4;5] 4);;
(neme [1;2;3;4;5] 6);;
(neme [1;2;3;4;5] (-1));;

(* Exercice 51 *)
let rec renverse_aux (aList : int list) (aList2 : int list) : int list =
  (match aList with
      [] -> aList2
    | wElem::wList -> (renverse_aux wList (wElem::aList2))
  );;

let rec renverse (aList : int list) : int list =
  (renverse_aux aList []);;

(renverse (aleatoire 100000));; (* très rapide *)
(renverse (renverse [1;2;3;4;5;6;7;8;9]));;

(* Exercice 52 *)
type pile = Pile of int list;;

let pile_vide : pile = (Pile []);;

let est_pile_vide (aPile : pile) : bool =
  (let Pile(wList) = aPile in
     (match wList with
	 [] -> true
       |_ -> false
     )
  );;

let empile (aNumber : int) (aPile : pile) : pile =
  (let Pile(wList) = aPile in
    (Pile (aNumber::wList))
  );;

let depile (aPile : pile) : int*pile =
  (if (est_pile_vide aPile) then failwith "empty stack" 
    else 
      (match aPile with
	  (Pile[]) -> failwith "empty stack" (* case tested to avoid warning *)
	| (Pile(wElement::wList)) -> (wElement , Pile (wList))
      )
  );;

(* Exercice 53 *)
let rec test_depile (aPile : pile) : int list =
  (let (wNumber,wPile) = (depile aPile) in
    (if (est_pile_vide wPile) then [wNumber] else wNumber::(test_depile wPile))
  );;

let rec test_empile (aList : int list) (aPile : pile) : pile =
  ( match aList with
      [] -> aPile
    | wElement::wList -> let wPile = (empile wElement aPile) in (test_empile wList wPile) 
  );;

let fonc_test_pile (aList : int list) : bool =
  (let wPile = (test_empile aList pile_vide) in
   (let wList = (renverse (test_depile wPile)) in
     aList=wList
   )
  );;

(fonc_test_pile (aleatoire 100000));;


(* Exercice 54 *)
let rec rac (aList : int list) : int*int list =
  (match aList with 
      [] -> failwith "empty list"
    | [wElement] -> (wElement,[])
    | wElement::wList -> let (wNumber,wList2) = (rac wList) in (wNumber,(wElement::wList2))
);;

(* Exercice 55 *)
type file = File of int list;;

let file_vide : file = (File []);;

let est_file_vide (aFile:file) : bool = 
  (let File(wList) = aFile in 
   (match wList with
    | [] -> true
    | _ -> false
   )
  );;

let enfile (aNumber : int) (aFile : file) : file =
  (let (File(wList))= aFile in
    File(aNumber::wList)
  );;

let defile (aFile : file) : int * file =
  (if (est_file_vide aFile) then failwith "empty queue" 
   else
    (let (File (wList)) = aFile in
     (match (renverse wList) with
	 [] -> failwith "emtpy queue" (* case tested to avoid warning *)
       | wElement::wList2 -> (wElement,(File(renverse wList2)))
     )
    )
  );;

(* Exercice 56 *)
let rec test_defile (aFile : file) : int list =
  (let (wNumber,wFile) = (defile aFile) in
   (if (est_file_vide wFile) then [wNumber] else wNumber::(test_defile wFile))
  );;

let rec test_enfile (aList : int list) (aFile : file) : file =
  (match aList with
      [] -> aFile
    | wElement::wList -> let wFile = (enfile wElement aFile) in (test_enfile wList wFile) 
  );;

let fonc_test_file (aList : int list) : bool =
  (let wFile = (test_enfile aList file_vide) in
    (let wList = (test_defile wFile) in
     aList=wList
    )
  );;

fonc_test_file (aleatoire 1000);;

(* Exercice 54 -> 56 with file of type (int list*int list) *)
type file = File of int list * int list;;

let file_vide : file = (File ([],[]));;

let est_file_vide_d (aFile:file) : bool = 
(
  let File(wListG,wListD) = aFile in 
  (if wListD=[] then true else false)
);;

let est_file_vide_g (aFile:file) : bool = 
( let File(wListG,wListD) = aFile in 
  (if wListG=[] then true else false)
);;

let est_file_vide (aFile:file) : bool =
  ((est_file_vide_d aFile) && (est_file_vide_g aFile));;

let enfile (aNumber : int) (aFile : file) : file =
  (let (File(wListG,wListD))= aFile in
   File(aNumber::wListG, wListD)
  );;

let retourne_file (aFile : file) : file =
  (if (est_file_vide_d aFile) then let File(wListG,wListD) = aFile in File([], (renverse wListG))
   else aFile
  );;

let defile (aFile : file) : int * file =
  (if (est_file_vide aFile) then failwith "empty file" 
   else
      (let File(aListG,wListD) = (retourne_file aFile) in
	 match wListD with
	     [] -> failwith "emtpy list" (* case tested to avoid warning *)
	   | wElement::wList -> (wElement, File(aListG,wList))
      )
  );;

let rec test_defile (aFile : file) : int list =
  (let (wNumber,wFile) = (defile aFile) in
   (if (est_file_vide wFile) then [wNumber] else wNumber::(test_defile wFile))
  );;

let rec test_enfile (aList : int list) (aFile : file) : file =
  (match aList with
      [] -> aFile
    | wElement::wList -> let wFile = (enfile wElement aFile) in (test_enfile wList wFile) 
  );;

let fonc_test_file (aList : int list) : bool =
  (let wFile = (test_enfile aList file_vide) in
    (let wList = (test_defile wFile) in
     wList=aList
    )
  );;

fonc_test_file (aleatoire 1000);;
