open Pure_regexp_out
open Dfa

(*
  number of states: Z.t,
  transition function: (dom: Z.t, cod: Z.t) -> pure_regexp
*)
type gnfa = GNFA of Z.t * ((Z.t * Z.t) -> pure_regexp_out)

let of_dfa alphabet_list (DFA(m, d, qi, acc)) =
  let d' = function
    | (_, q2) when (q2 <= Z.(~$0) || q2 > Z.(m + ~$1)) -> raise (Invalid_argument "")
    | (q1, _) when (q1 < Z.(~$0) || q1 >= Z.(m + ~$1)) -> raise (Invalid_argument "")
    | (q1, q2) when (q1 = Z.(~$0)) -> if q2 = (Z.(qi + ~$1)) then Concat [] else Alter []
    | (q1, q2) when (q2 = Z.(m + ~$1)) -> if acc (Z.(q1 - ~$1)) then Concat [] else Alter []
    | (q1, q2) -> (Alter (List.map (fun a -> Char a) (List.filter (fun a -> (d (Z.(q1 - ~$1), a)) = Z.(q2 - ~$1)) alphabet_list)))
  in GNFA(Z.(m + ~$2), d')
