let explode s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let zip2 l1 l2 =
  let rec zip2' l1 l2 l =
    match (l1, l2) with
      | ([], []) -> l
      | ((l1h::l1t), (l2h::l2t)) -> zip2' l1t l2t ((l1h, l2h)::l)
      | _ -> l
  in zip2' l1 l2 [] |> List.rev

let unzip3 l =
  let rec unzip3' l1 l2 l3 = function
    | [] -> (l1, l2, l3)
    | (v1, v2, v3)::t -> unzip3' (v1::l1) (v2::l2) (v3::l3) t
  in unzip3' [] [] [] l |> function (l1, l2, l3) -> (List.rev l1, List.rev l2, List.rev l3)

let sum = List.fold_left (+) 0

let last xs = List.fold_left (fun _ x -> Some x) None xs

let init l = l |> List.rev |> List.tl |> List.rev

let hd_opt l = try Some (List.hd l) with Failure(_) -> None

let rec power b n = if n <= 0 then 1 else b * power b (n - 1)

let rec range m n = if m >= n then [] else m :: (range (m + 1) n)

let product xs ys = List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

let replace input output =
    Str.global_replace (Str.regexp_string input) output