open Util

(*
  number of states: Z.t,
  transition function: (dom: Z.t, alphabet: char) -> cod: Z.t
  initial state: Z.t,
  acceptance predicate: Z.t -> bool,
*)
type dfa = DFA of Z.t * ((Z.t * char) -> Z.t) * Z.t * (Z.t -> bool)

let push_if_unseen x xs = if List.mem x xs then (xs, false) else (xs @ [x], true)

let rec scan alphabet_list d states = 
  let scan1 alphabet_list d states = List.fold_left (fun (states, b) (q, a) -> let (states', is_changed) = push_if_unseen (d (q, a)) states in (states', b || is_changed)) (states, false) (product states alphabet_list)
  in let (states', is_changed) = scan1 alphabet_list d states in if is_changed then scan alphabet_list d states' else states'

let rec zlength = function
  | [] -> Z.(~$0)
  | (_::xs) -> Z.(~$1 + zlength xs)

let rec znth ls n = match (ls, n) with
  | ([], _) -> raise Not_found
  | (x::_, n) when (n = Z.(~$0)) -> x
  | (_::xs, n) -> znth xs (Z.(n - ~$1))

let rec zindex x = function
  | [] -> raise Not_found
  | h::t -> if h = x then Z.(~$0) else Z.(~$1 + zindex x t)

let minimize alphabet_list (DFA(_, d, qi, acc)) =
  let states = scan alphabet_list d [qi]
  in DFA(zlength states, (fun (q, a) -> zindex (d (znth states q, a)) states), Z.(~$0), (fun q -> acc (znth states q)))
