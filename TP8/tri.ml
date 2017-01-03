module type TTri = functor (E : Ordre.TypeOrdonne) ->
sig
  val trier : E.t list -> E.t list
end

module Bubblesort : TTri = functor (E : Ordre.TypeOrdonne) ->
struct
  let rec bulles (l:E.t list):E.t list*bool= match l with
  |[]->([],false)
  |[x]->(l,false)
  |t1::t2::q->
    if (E.plus_grand t1 t2) then
      let (a,_)=(bulles (t1::q)) in (t2::a,true)
    else
      let (a,b)=(bulles (t2::q)) in (t1::a,b)
  let rec (trier : E.t list -> E.t list) = fun l ->
    match l with
    |[] -> []
    |_ -> let (a,b) = (bulles l) in if b then (trier a) else a
end

module Quicksort : TTri = functor (E : Ordre.TypeOrdonne) ->
struct
  let rec decoupe p l= match l with
  |[]->([],[])
  |t::q->
    let (petit,grand)= (decoupe p q)
    in
    if E.plus_grand p t then
      (t::petit, grand)
    else
      (petit, t::grand)
  let rec (trier : E.t list -> E.t list) = fun l ->
    match l with
    |[]->[]
    |x::[]->[x]
    |t::q->
      let (p,g)= (decoupe t q)
      in (trier p)@[t]@(trier g);;
end


