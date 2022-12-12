open Util

type pure_regexp_in =
  | Chars of char list
  | String of char list
  | Alter of pure_regexp_in list
  | Concat of pure_regexp_in list
  | Star of pure_regexp_in

(* type pure_regexp_in =
  | Chars of char list
  | String of char list
  | Alter of pure_regexp_in list
  | Concat of pure_regexp_in list
  | Star of pure_regexp_in
  | Failure *)

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

(* let a = List.exists (fun x -> bool_of_string x |> string_of_bool |> bool_of_string ) ["true"; "false"] *)

let string_of_regexp alphabet_set =
  let rec of_regexp' = function
    | Parser.Char(c) -> Char.escaped c
    | Parser.String(s) -> s
    | Parser.CharClass(s, b) -> "(" ^ ((if b then Parser.Charset.minus alphabet_set s else s)
                                        |> Parser.Charset.explode
                                        |> List.map Char.escaped
                                        |> String.concat "|") ^ ")"
    | Parser.Seq(rs) -> "(" ^ (rs |> List.map of_regexp'
                                  |> String.concat "") ^ ")"
    | Parser.Alt(r1, r2) -> "(" ^ (of_regexp' r1) ^ (of_regexp' r2) ^ ")"
    | Parser.Star(r) -> "(" ^ (of_regexp' r) ^ ")*"
    | Parser.Plus(r) -> "(" ^ (of_regexp' r) ^ ")+"
    | Parser.Option(r) -> "(" ^ (of_regexp' r) ^ ")?"
    | Parser.Group(_, r) -> of_regexp' r (* back-reference *)
    | Parser.Refgroup(_) -> raise (Failure "") (* back-reference *)
    | Parser.Bol -> raise (Failure "") (* ?? *)
    | Parser.Eol -> raise (Failure "") (* ?? *)
    | Parser.Wordboundary -> raise (Failure "") (* ?? *)
in of_regexp'

(* let of_regexp_for_failure alphabet_set =
  let rec of_regexp' = function
    | Parser.Char(c) -> Chars [c]
    | Parser.String(s) -> String (explode s)
    | Parser.CharClass(s, b) -> Chars ((if b then Parser.Charset.minus alphabet_set s else s)
                                                |> Parser.Charset.explode
                                                |> List.map Char.escaped
                                                |> String.concat "|"
                                                |> explode)
    | Parser.Seq(rs) -> if List.exists (fun x -> contains x "(?!.).") (List.map (string_of_regexp alphabet_set) rs) 
                        then raise (Failure "a")
                        else Concat (List.map of_regexp' rs)
    (* | Parser.Seq(rs) -> if List.exists (fun x -> contains x "(?!.).") (List.map (string_of_regexp alphabet_set) rs) 
                          then let () = List.iter print_endline (List.map (string_of_regexp alphabet_set) rs) in raise (Failure "a")
                          else let () = List.iter print_endline (List.map (string_of_regexp alphabet_set) rs) in Concat (List.map of_regexp' rs) *)
    | Parser.Alt(r1, r2) -> if contains (string_of_regexp alphabet_set r1) "(?!.)."
                              && contains (string_of_regexp alphabet_set r2) "(?!.)."
                            then raise (Failure "b") else Alter [of_regexp' r1; of_regexp' r2]
    | Parser.Star(r) -> if contains (string_of_regexp alphabet_set r) "(?!.)." then Concat [] else let () = string_of_regexp alphabet_set r |> print_endline in Star (of_regexp' r)
    | Parser.Plus(r) -> let pr = of_regexp' r in Concat [pr; Star(pr)]
    | Parser.Option(r) -> Alter [Concat []; of_regexp' r]
    | Parser.Group(_, r) -> of_regexp' r (* back-reference *)
    | Parser.Refgroup(_) -> raise (Failure "") (* back-reference *)
    | Parser.Bol -> raise (Failure "") (* ?? *)
    | Parser.Eol -> raise (Failure "") (* ?? *)
    | Parser.Wordboundary -> raise (Failure "") (* ?? *)
in of_regexp' *)




(* add *)

let rec string_of_pure_regexp_in = function
  | Chars c -> "(" ^ List.fold_left (fun x y -> x ^ (Char.escaped y)) "" c ^ ")"
  | String s -> "(" ^ List.fold_left (fun x y -> x ^ (Char.escaped y)) "" s ^ ")"
  | Alter r -> "(" ^ String.concat "|" (List.map string_of_pure_regexp_in r) ^ ")"
  | Concat r -> "(" ^ String.concat "" (List.map string_of_pure_regexp_in r) ^ ")"
  | Star r -> "(" ^ string_of_pure_regexp_in r ^ ")*"

(* let rec delete_failure = function
  | Chars c -> Chars c
  | String s -> String s
  | Alter r -> if 
  | Concat r -> 
  | Star r ->  *)