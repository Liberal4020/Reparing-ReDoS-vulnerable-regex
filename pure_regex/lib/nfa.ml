module IntSet = Set.Make(Int)

(*
  number of states: int,
  transition function: (dom: int, alphabet: char option) -> cod: IntSet.t,
  set of acceptable states: IntSet.t
*)
type nfa = NFA of int * ((int * char option) -> IntSet.t) * IntSet.t
