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

module Entier : TypeOrdonne

module Chaine : TypeOrdonne
