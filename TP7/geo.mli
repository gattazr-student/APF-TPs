(** Géométrie. *)

(** Valeur approchée de Pi. *)
val pi : float

(** Point ou vecteur. *)
type point = { x : float; y : float; }

(** Surface. *)
type surface =
  | Circle of point * float (** Cercle : centre et rayon *)
  | Line of point * point   (** Segment : deux extremites *)
  | Polygon of point list   (** Polygone plein : listes des sommets *)

  (** Constructeur de point. *)
val point : float -> float -> point

(** Opérateurs infixes sur les vecteurs. *)

val ( +| ) : point -> point -> point (** Translation. *)
val ( -| ) : point -> point -> point (** Translation inverse. *)
val ( ~| ) : point -> point (** Inverse. *)
val ( *| ) : float -> point -> point (** Homothétie. *)

val milieu : point -> point -> point
(** Vecteur normal d'un vecteur. *)
val normal : point -> point
(** Produit scalaire de deux vecteurs. *)
val dot : point -> point -> float
(** Carré scalaire d'un vecteur. *)
val sqdot : point -> float
(** Norme d'un vecteur. *)
val length : point -> float
(** Vecteur unitaire d'un vecteur. *)
val unitise : point -> point

(* Distance entre deux points. *)
val distance : point -> point -> float
(* Ajoute un angle à un vecteur. *)
val rotate : point -> float -> point
