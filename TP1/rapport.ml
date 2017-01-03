(* TP 1 *)

(* TP réalisé par Rémi GATTAZ et Lucas GUERRY *)

(* Exercice 1 *)
let r = let x = 7 in 6 * x ;; 	-> int (42)
let a = (r - 6) / 6 - 6 ;; 		-> int (0)
let o = r * r - x * x - 51 ;;	-> x is undefined
let u = let x = 9 in if (x < 9) then 9 / (x - x) else (x + x) / 9 ;; -> int (2)
let l = let x,y = 3 + 4,6 in x * y;; -> int (42)

(* Exercice 2 *)
let f1 = fun x -> fun y -> fun z -> x * y * z ;;
let f1 = fun x -> fun y -> fun z -> x + y + z ;;

(* Exercice 3 *)
(* 
- la fonction multiply est du type int -> int -> int
- le prédicat isGreaterThan est du type int -> int -> bool
- f1 et f1 sont du type int -> int -> int
*)

let f3 = fun x -> fun y -> x (y + 1) || false
(* 
- f3 est du type (int -> bool) -> int -> bool.
- y est un entier (puisqu'on a y+1)
- x est une fonction qui à un paramètre int (y+1) et retourne un bool
*)
 
let f4 = fun x -> fun y -> x (y + 1) + 1
(*
- f4 est du type (int -> int) -> int -> int
- y est en tier (puisqu'on a y+1)
- x est une fonction qui à un paramètre int (y+1) et retourne un entier
*)

let f5 = fun x -> x x
(*
- La fonction n'est pas évaluable.
- La première fonction x est une fonction qui demande un paramètre. Or la seconde fonction x n'en a pas.
*)

(* Exercice 4 *)
let f = fun i -> fun x -> x + i
(* int -> int -> int *)
let g = f 2
(* int -> int *)

let g1 = f1 1;;
let g2 = f2 0;;

g1 2 3 ;;
g2 2 3 ;;


(* Exercice 5 *)
let isPositif x = x >= 0;;
let isEven x = (x mod 2) = 0 ;;
let isPythagoricien (x:float) (y:float) (z:float) = let x2 = x**2. and y2 = y**2. and z2 = z**2. in
						    x2=y2+.z2 || y2=x2+.z2 || z2=x2+.y2;;
let isSameSign (x:float) (y:float) = (x < 0. && y < 0.) || (x > 0. && y > 0.) || (x = 0. || y = 0.);;

(* Exercice 6 *)
let min2entiers (x:int) (y:int) = (if x < y then x else y);;

(* Exercice 7 *)
let max2entiers (x:int) (y:int) = (if x > y then x else y);;

(* Exercice 8 *)
let max3entiers (x:int) (y:int) (z:int)= (max2entiers (max2entiers x y) z);;

(* Exercice 9 *)
type semaine = Lundi|Mardi|Mercredi|Jeudi|Vendredi|Samedi|Dimanche ;;
let isWeekend (j:semaine) = match j with
  | Samedi | Dimanche -> true
  |_ -> false ;;

(* Exercice 10 *)
let aireCarre (a:float) = a**2. ;;
let aireRectangle (a:float) (b:float) = a *. b ;;
let aireCercle (r:float) = 2. *. 3.14 *. r**2. ;;
let aireTriangleRect (a:float) (h:float) = let b = sqrt( h**2. -. a**2.) in a *. b /. 2. ;;

(* Exercice 11 *)
type figure = Carre of float | Rectangle of float*float | Cercle of float ;;
let aireFigure (x:figure) = match x with
	| Carre(a) -> (aireCarre a)
	| Rectangle(a,b) -> (aireRectangle a b) 
	| Cercle(a) -> (aireCercle a) ;;

(* Exercice 12 *)
type complexe = Cplx float * float ;;
let neutre = Cplx(0., 0.);;
let addCplx (x:complexe) (y:complexe) = 
  let Cplx(re1,im1) = x and Cplx(re2, im2) = y in
  Cplx(re1 +. re2, im1 +. im2 );;
let modCplx (x:complexe) = let Cplx(re,im) = x in
	sqrt(re**2. +. im**2.);;
let oppCplx (x:complexe) = let Cplx(re,im) = x in
	Cplx(-.re, -.im);;

(* Exercice 13 *)
type point2D = Point2D of float * float ;;
let distance (a:point2D) (b:point2D) = let Point2D(x1,y1) = a and Point2D(x2,y2) = b in
	sqrt( (x1-.x2)**2. +. (y1-.y2)**2. ) ;;
(distance Point2D(0., 0.) Point2D(2., 2.)) == sqrt(2.) ;;

type segment = Segment of point2D * point2D ;;
let milieu (a:segment) = let Segment(Point2D(x1,y1),Point2D(x2,y2)) = a in
	Point2D( x1 +. (x2-.x1)/.2., y1 +. (y2-.y1)/.2.);;
(milieu (Segment(Point2D(0., 0.), Point2D(2., 2.)))) = (Point2D (1., 1.)) ;;

type vecteur = Vecteur of float*float;;
let getVecteur (a:point2D) (b:point2D) = let Point2D(x1,y1) = a and Point2D(x2,y2) = b in
					 Vecteur(x2-.x1, y2-.y1);;
(getVecteur (Point2D (-.1., -.1.)) (Point2D (1., 1.))) = Vecteur(2., 2.);;

type droite = Droite of point2D * vecteur;;
