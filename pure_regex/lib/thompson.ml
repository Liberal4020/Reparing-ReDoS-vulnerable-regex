open Pure_regexp_in
open Nfa
open Util

module IntSet = Set.Make(Int)

let shift n d = fun (q, a) -> IntSet.map ((+) n) (d ((q - n), a))

(* sum_list [a; b; c; ...] = [a; a + b; a + b + c; ...] *)
let sum_list l = l |> List.fold_left (fun sums n -> (sums |> hd_opt |> (Option.value ~default: 0)) + n :: sums) [] |> List.rev

let bigunion = List.fold_left IntSet.union IntSet.empty

let rec thompson = function
  | Chars(cs) ->
      let m = 2 in
      let d = function
        | (0, Some a) when (List.mem a cs) -> IntSet.singleton 1
        | (q, _) -> if 0 <= q && q < m
          then IntSet.empty
          else raise (Invalid_argument (string_of_int q))
      in let f = IntSet.singleton 1
      in NFA(m, d, f)
  | String(cs) ->
      let m = List.length cs + 1 in
      let d = function
        | (q, _) when (q = m - 1) -> IntSet.empty
        | (q, Some a) when (a = List.nth cs q) -> IntSet.singleton (q + 1)
        | (q, _) -> if 0 <= q && q < m
          then IntSet.empty
          else raise (Invalid_argument (string_of_int q))
      in let f = IntSet.singleton (m - 1)
      in NFA(m, d, f)
  | Alter(rs) ->
      let mdfs = List.map (fun r -> let (NFA(m, d, f)) = thompson r in (m, d, f)) rs in
      let (ms, ds, fs) = unzip3 mdfs in
      let offsets = 0::(sum_list ms) |> init |> List.map ((+) 1) in
      let m = sum ms + 1 in
      let rec d' mds = match mds with
        | [] -> (fun (q, _) -> raise (Invalid_argument (string_of_int q)))
        | (mh, dh)::t -> (fun (q, a) -> if 0 <= q && q < mh then dh (q, a) else (shift mh (d' t)) (q, a))
      in let d = function
        | (0, None) -> IntSet.of_list offsets
        | (0, _) -> IntSet.empty
        | (q, a) -> if 1 <= q && q < m
            then (shift 1 (d' (zip2 ms ds))) (q, a)
            else raise (Invalid_argument (string_of_int q))
      in let f = zip2 offsets fs |> List.map (fun (offset, f') -> IntSet.map ((+) offset) f') |> bigunion
      in NFA(m, d, f)
  | Concat(rs) ->
      let mdfs = List.map (fun r -> let (NFA(m, d, f)) = thompson r in (m, d, f)) rs in
      let (ms, _, _) = unzip3 mdfs in
      let m = sum ms + 1 in
      let rec d' mdfs = match mdfs with
        | [] -> (function
            | (0, None) -> IntSet.singleton 0
            | (q, _) -> if q = 0
                then IntSet.empty
                else raise (Invalid_argument (string_of_int q))
            )
        | (mh, dh, fh)::t -> (fun (q, a) -> if 0 <= q && q < mh
            then if (IntSet.mem q fh) && (a = None) then IntSet.add mh (dh (q, a)) else dh (q, a)
            else (shift mh (d' t)) (q, a)
            )
      in let d = d' mdfs
      in let f = IntSet.singleton (m - 1)
      in NFA(m, d, f)
  | Star(r) ->
      let (NFA(m', d', f')) = thompson r in
      let m = m' + 1 in
      let d = function
        | (0, None) -> IntSet.singleton 1
        | (0, _) -> IntSet.empty
        | (q, a) -> if 1 <= q && q < m
            then if (IntSet.mem (q - 1) f') && (a = None) then IntSet.add 0 ((shift 1 d') (q, a)) else (shift 1 d') (q, a)
            else raise (Invalid_argument (string_of_int q))
      in let f = IntSet.add 0 (IntSet.map ((+) 1) f')
      in NFA(m, d, f)
