module type TTri = functor (E : Ordre.TypeOrdonne) ->
sig
  val trier : E.t list -> E.t list
end

module Bubblesort : TTri

module Quicksort : TTri
