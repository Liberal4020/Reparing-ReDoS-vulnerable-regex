open Util

type pure_regexp_in =
  | Chars of char list
  | String of char list
  | Alter of pure_regexp_in list
  | Concat of pure_regexp_in list
  | Star of pure_regexp_in

(* modify *)

let of_regexp alphabet_set =
  let rec of_regexp' = function
    | Parser.Char(c) -> Chars [c]
    | Parser.String(s) -> String (explode s)
    | Parser.CharClass(s, b) -> Chars ((if b then Parser.Charset.minus alphabet_set s else s)
                                        |> Parser.Charset.explode
                                        |> List.map Char.escaped
                                        |> String.concat "|"
                                        |> explode)
    (* | Parser.CharClass(s, b) -> Chars ((if b then Parser.Charset.minus alphabet_set s else s) |> Parser.Charset.explode) *)
    | Parser.Seq(rs) -> Concat (List.map of_regexp' rs)
    | Parser.Alt(r1, r2) -> Alter [of_regexp' r1; of_regexp' r2]
    (* | Parser.Star(r) -> Star ((of_regexp' r) |> List.cons '(' |> List.rev |> List.cons ')' |> List.rev) *)
    | Parser.Star(r) -> Star (of_regexp' r)
    | Parser.Plus(r) -> let pr = of_regexp' r in Concat [pr; Star(pr)]
    | Parser.Option(r) -> Alter [Concat []; of_regexp' r]
    | Parser.Group(_, r) -> of_regexp' r (* back-reference *)
    | Parser.Refgroup(_) -> raise (Failure "") (* back-reference *)
    | Parser.Bol -> raise (Failure "") (* ?? *)
    | Parser.Eol -> raise (Failure "") (* ?? *)
    | Parser.Wordboundary -> raise (Failure "") (* ?? *)
in of_regexp'


(* add *)

let rec string_of_pure_regexp_in = function
  | Chars c -> "(" ^ List.fold_left (fun x y -> x ^ (Char.escaped y)) "" c ^ ")"
  | String s -> "(" ^ List.fold_left (fun x y -> x ^ (Char.escaped y)) "" s ^ ")"
  | Alter r -> "(" ^ String.concat "|" (List.map string_of_pure_regexp_in r) ^ ")"
  | Concat r -> "(" ^ String.concat "" (List.map string_of_pure_regexp_in r) ^ ")"
  | Star r -> "(" ^ string_of_pure_regexp_in r ^ ")*"