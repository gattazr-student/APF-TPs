module type TypeOrdonne =
sig
  type t
  val plus_grand : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t
    (* Les fonctions to_string et of_string sont nécessaires
       pour manipuler des éléments de type t passés en entrée
       du programme main.ml par l'utilisateur.
    *)
end

module Entier : TypeOrdonne =
struct
  type t = int
  let (plus_grand : t -> t -> bool) = fun x y -> x>y
  let (to_string : t -> string) = fun x ->
    string_of_int x
  let (of_string : string -> t) = fun s ->
    int_of_string s
end

module Chaine : TypeOrdonne =
struct
  type t = string
  let (plus_grand : t -> t -> bool) = fun ch1 ch2 ->
    if (compare ch1 ch2)=1 then true
    else false
  let (to_string : t -> string) = fun ch -> ch
  let (of_string : string -> t) = fun s -> s
end
