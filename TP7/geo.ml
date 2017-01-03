(* TP7 *)

(* TP réalisé par Jules HABLOT et Rémi GATTAZ *)

(** pi **)
let pi = 4. *. atan 1.;;

(** Point ou vecteur. *)
type point = { x : float; y : float; };;

(** Surface. *)
type surface =
  | Circle of point * float (** Cercle : centre et rayon *)
  | Line of point * point   (** Segment : deux extremites *)
  | Polygon of point list   (** Polygone plein : listes des sommets *)

(** Constructeur de point. *)
let point (a : float) (b : float):point = {x=a; y=b};;

let ( +| ) (a : point) (b : point):point = {x=(a.x +. b.x); y=(a.y +. b.y)};;
let ( -| ) (a : point) (b : point):point = {x=(a.x -. b.x); y=(a.y -. b.y)};;
let ( ~| ) (a : point) : point = {x=a.y ; y=a.x};;
let ( *| ) (h : float) (a : point) : point = {x=a.x*.h; y=a.y*. h};;

(* let pa = (point 1. 1.);; *)
(* let pb = (point 2. 3.);; *)

(* pa +| pb;; *)
(* pa -| pb;; *)
(* ~| pb;; *)
(* 3. *| pb;; *)

let milieu (a:point)(b:point):point = {x = (a.x+.b.x)/.2. ; y = (a.y+.b.y)/.2. } ;;
(* let pc = (point 3. 3.);; *)
(* milieu pa pc ;; *)

let normal (v:point):point = {x= v.y ;y= -.v.x };;
let dot (a:point)(b:point):float= a.x*.b.x +. a.y*.b.y ;;
let sqdot (a:point):float= dot a a;;
let length (v:point):float= sqrt( v.x**2. +. v.y**2.);;
let unitise (v:point):point=let l=length v in {x= v.x/.l;y= v.y/.l};;

(* let pd = point 5. 2.;; *)
(* length (unitise pd) ;; *)

let distance (a : point) (b : point) : float = sqrt( (a.x -. b.x)**2. +. (a.y -. b.y)**2.);;

(* let origine = (point 0. 0.);; *)
(* (distance pa origine);; *)

let rotate (a : point) (f : float) : point = {x=a.x*.(cos f) -. a.y*.(sin f); y=a.x*.(sin f) +. a.y*.(cos f)};;

(* (rotate pa pi);; *)
