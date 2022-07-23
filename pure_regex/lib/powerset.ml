open Nfa
open Dfa
open Util

module IntSet = Set.Make(Int)

(* bigunion f { a, b, c, ... } = (f a) ∪ (f b) ∪ (f c) ∪ ... *)
let bigunion f s = IntSet.fold (fun n s -> IntSet.union (f n) s) s IntSet.empty

let rec closure f s = let s' = f s in if IntSet.equal s s' then s else closure f s'

let empty_closure (NFA(_, d, _)) = closure (fun qs -> IntSet.union (bigunion (fun q -> d (q, None)) qs) qs)

let bits_to_set n b = IntSet.of_list (List.filter (fun i -> Z.(b land (~$1 lsl i) <> ~$0)) (range 0 n))

let set_to_bits s = IntSet.fold (fun n -> (Z.(+)) (Z.(~$2 ** n))) s (Z.(~$0))

let powerset (NFA(m, d, f)) =
  let m' = Z.(~$2 ** m)
  in let d' (q, a) = set_to_bits (empty_closure (NFA(m, d, f)) (bigunion (fun q -> d (q, Some a)) (bits_to_set m q)))
  in let qi = set_to_bits (empty_closure (NFA(m, d, f)) (IntSet.singleton 0))
  in let acc = fun k -> not (IntSet.is_empty (IntSet.inter f (bits_to_set m k)))
  in DFA(m', d', qi, acc)
