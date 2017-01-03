(* TP5 *)

(* TP réalisé par Jules HABLOT et Rémi GATTAZ *)

(* Exercice Préliminaire *)
type quadtree = Feuille of int
		| Noeud of quadtree * quadtree * quadtree * quadtree;;
let whiteF = (Feuille 255);;
let blackF = (Feuille 0);;

let diamondSwordTree = (Noeud(
			(Noeud (
			  whiteF,
			  whiteF,
			  (Noeud (whiteF,whiteF,blackF,whiteF)),
			  (Noeud (whiteF,whiteF,blackF,blackF))
			 )
			),
			(Noeud (
			  (Noeud (whiteF,whiteF,blackF,whiteF)),
			  blackF,
			  (Noeud (blackF,whiteF,whiteF,whiteF)),
			  blackF
			 )
			),
			(Noeud (
			  (Noeud (blackF,whiteF,whiteF,whiteF)),
			  whiteF,
			  whiteF,
			  (Noeud (blackF,whiteF,whiteF,blackF))
			 ) 
			),
			(Noeud (
			  (Noeud (whiteF,blackF,whiteF,whiteF)),
			  blackF,
			  (Noeud (whiteF,blackF,whiteF,whiteF)),
			  (Noeud (whiteF,blackF,whiteF,blackF))
			 ) 
			)
)
);;

let makeDiamondSwordArray =
(let diamondArray = Array.make_matrix 8 8 255 in
 (diamondArray).(0).(6) <- 0;
 (diamondArray).(0).(7) <- 0;
 (diamondArray).(1).(5) <- 0;
 (diamondArray).(1).(6) <- 0;
 (diamondArray).(1).(7) <- 0;
 (diamondArray).(2).(4) <- 0;
 (diamondArray).(2).(5) <- 0;
 (diamondArray).(2).(6) <- 0;
 (diamondArray).(3).(0) <- 0;
 (diamondArray).(3).(1) <- 0;
 (diamondArray).(3).(3) <- 0;
 (diamondArray).(3).(4) <- 0;
 (diamondArray).(3).(5) <- 0;
 (diamondArray).(4).(1) <- 0;
 (diamondArray).(4).(2) <- 0;
 (diamondArray).(4).(3) <- 0;
 (diamondArray).(4).(4) <- 0;
 (diamondArray).(5).(2) <- 0;
 (diamondArray).(5).(3) <- 0;
 (diamondArray).(6).(1) <- 0;
 (diamondArray).(6).(3) <- 0;
 (diamondArray).(6).(4) <- 0;
 (diamondArray).(7).(0) <- 0;
 (diamondArray).(7).(4) <- 0;
 diamondArray
)

let diamondSwordArray = makeDiamondSwordArray;;

(* Exercice 68 *)
let rec rot_pos (aTree : quadtree) : quadtree = (
  (match aTree with
    Noeud(wA,wB,wC,wD) -> 
      (Noeud (
	(rot_pos wB),
	(rot_pos wC),
	(rot_pos wD),
	(rot_pos wA)
       )
      )
  | _ -> aTree
  )
);;

(* Exercice 67 *)
let rec rot_neg (aTree : quadtree) : quadtree = (
  (match aTree with
    Noeud(wA,wB,wC,wD) -> 
      (Noeud (
	(rot_pos wD),
	(rot_pos wA),
	(rot_pos wB),
	(rot_pos wC)
       )
      )
  | _ -> aTree
  )
);;

(rot_neg(rot_pos(Noeud(whiteF,whiteF,blackF,blackF))));;

(* Exercice 68 *)
let rec miroir_hori (aTree : quadtree) : quadtree =
  (match aTree with
    Noeud(wA,wB,wC,wD) -> 
      (Noeud (
	(miroir_hori wD),
	(miroir_hori wC),
	(miroir_hori wB),
	(miroir_hori wA)
       )
      )
  |_ -> aTree
  );;

(miroir_vert(miroir_vert(Noeud(whiteF,whiteF,blackF,blackF))));;

(* Exercice 69 *)

let rec miroir_vert (aTree : quadtree) : quadtree =
  match aTree with 
  |Noeud(wA,wB,wC,wD) -> 
    (Noeud (
      (miroir_vert wB),
      (miroir_vert wA),
      (miroir_vert wD),
      (miroir_vert wC)))
  |_ -> aTree
;;

(miroir_vert(miroir_vert(Noeud(whiteF,whiteF,blackF,blackF))));;

(* Exercice 70 *)
let rec invert_video (aTree : quadtree) (aMax : int) : quadtree =
  (match aTree with 
  |Noeud(wA,wB,wC,wD) -> 
    (Noeud (
      (invert_video wA aMax),
      (invert_video wB aMax),
      (invert_video wC aMax),
      (invert_video wD aMax)
     )
    )
  | Feuille(wValue) -> 
    (if wValue = 0 then (Feuille aMax)
    else 
      (Feuille (aMax - wValue)
      )
    )
  );;

(diamondSwordTree);;
(invert_video diamondSwordTree 255);;
let arbre = 
  (Noeud 
   ( 
     (Noeud ( (Feuille 56), (Feuille 34), (Feuille 120), (Feuille 1))),
     (Feuille 0), 
     (Feuille 100), 
     (Feuille 85)
   )
  );;

(invert_video arbre 255);;

(* Exercice 71 *)

let max a b = if a<b then b else a ;;

let rec max_gris (aTree : quadtree) : int =
  match aTree with
  |Feuille(w) -> w
  |Noeud(wA,wB,wC,wD) ->
	 let ma=(max_gris wA) and mb=(max_gris wB) and mc=(max_gris wC) and md=(max_gris wD) in
	 max (max ma mb) (max mc md)
;;

max_gris arbre;;

(* Exercice 72 *)
let rec min_quad (aTree : quadtree) : quadtree =
  (match aTree with
  | (Noeud (wA, wB, wC, wD)) -> 
    let minA=(min_quad wA) and minB=(min_quad wB) and minC=(min_quad wC) and minD=(min_quad wD) in
    (match (minA,minB,minC,minD) with
    | Feuille(w1),Feuille(w2),Feuille(w3),Feuille(w4) ->
	  if (w1 = w2 && w2 = w3 && w3 = w4) then
	    (Feuille w1)
	  else (Noeud (minA, minB, minC, minD))
    |_ -> (Noeud (minA, minB, minC, minD))
    )
  |_ -> aTree
    );;

let arbre2 = 
  (Noeud 
   ( 
     (Noeud ( (Feuille 56), (Feuille 56), (Feuille 56), (Feuille 56))),
     (Feuille 56), 
     (Feuille 56), 
     (Feuille 56)
   )
);;
(min_quad arbre2);;

(* Exercice 73 *)
let rec tab_vers_quad_aux (aT : int array array)(px,py,t:int*int*int):quadtree=
  match t with
  |1 -> Feuille (aT.(px).(py))
  |_ -> let newt=t/2 in 
	Noeud(
	  (aux aT (px,py,newt)),
	  (aux aT (px,(py+newt),newt)),
	  (aux aT ((px+newt),(py+newt),newt)),
	  (aux aT ((px+newt),py,newt))
	)
;;

let rec tab_vers_quad (aT : int array array):quadtree =
  min_quad(tab_vers_quad_aux aT (0,0,Array.length(aT)));;

let test = [|
[|1;2;3;4|];
[|5;6;7;8|];
[|9;10;11;12|];
[|13;14;15;16|];
	   |];;
(tab_vers_quad test);;

(tab_vers_quad diamondSwordArray);;
diamondSwordTree;;
