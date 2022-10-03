type pure_regexp_out =
  | Char of char
  | Alter of pure_regexp_out list
  | Concat of pure_regexp_out list
  | Star of pure_regexp_out

let rec simplify = function
  | Char(c) -> Char c
  | Alter(rs) ->
      let rs' = List.map (fun r' -> match (simplify r') with
        | Alter(rs) -> rs
        | r -> [r]
        ) rs
      in let rs'' = List.flatten rs'
      in if List.length rs'' = 1 then List.hd rs'' else Alter rs''
  | Concat(rs) ->
      let rs' = List.map (fun r' -> match (simplify r') with
        | Concat(rs) -> rs
        | r -> [r]
        ) rs
      in let rs'' = List.flatten rs'
      in if List.mem (Alter []) rs'' then Alter [] else if List.length rs'' = 1 then List.hd rs'' else Concat rs''
  | Star(r) -> let r' = simplify r in if r' = Alter [] || r' = Concat [] then Concat [] else Star r'

let escape s = if (List.mem s ["$"; "^"; "."; "*"; "+"; "?"; "["; "]"; "("; ")"; "|"; "\\"]) then "\\" ^ s else s

let rec unparse = function
  | Char(c) -> c |> String.make 1 |> escape
  | Alter(rs) -> "(" ^ (rs |> List.map unparse |> String.concat "|") ^ ")"
  | Concat(rs) -> rs |> List.map unparse |> String.concat ""
  | Star(r) -> "(" ^ (unparse r) ^ ")*"
