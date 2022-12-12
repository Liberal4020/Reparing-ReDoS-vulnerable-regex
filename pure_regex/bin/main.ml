open Lib
(* "<span[^>]*font-style:italic[^>]*>" *)
(* let input = "a|b" *)

(* let input = read_line () *)

let read_file name =
  let ic = open_in name in
  let lines = ref [] in
  begin
    try while true do lines := input_line ic :: !lines done
    with End_of_file -> close_in ic
  end;
  List.rev !lines

let input1 = List.hd (read_file "./bin/regex1.txt")

let input2 = List.hd (read_file "./bin/regex2.txt")



(* let alphabet_set = Parser.Charset.union (Parser.Charset.range 'a' 'z') (Parser.Charset.of_list ['<'; '>'; ':'; '=']) *)

let alphabet_set = Parser.Charset.union (Parser.Charset.range 'a' 'z') (Parser.Charset.of_list ['-'; '<'; '>'; ':'; '='])


(* let if_is_failure global_string = if Util.contains global_string "(?!.)." then else *)

(* let pr_input1 = input1 |> Util.replace "?:" "" |> Util.replace "?!" "" |> Parser.parse |> Pure_regexp_in.of_regexp alphabet_set

let pr_input2 = input2 |> Util.replace "?:" "" |> Util.replace "?!" "" |> Parser.parse |> Pure_regexp_in.of_regexp alphabet_set *)

(* let pr_input1 = input1 |> Util.replace "?:" "" |> Parser.parse |> Pure_regexp_in.of_regexp_for_failure alphabet_set

let pr_input2 = input2 |> Util.replace "?:" "" |> Parser.parse |> Pure_regexp_in.of_regexp_for_failure alphabet_set *)

let pr_input1 = input1 |> Util.replace "?:" "" |> Parser.parse alphabet_set |> Pure_regexp_in.of_regexp alphabet_set

let pr_input2 = input2 |> Util.replace "?:" "" |> Parser.parse alphabet_set |> Pure_regexp_in.of_regexp alphabet_set


let oc = open_out "../regeq/src/input.txt"

let _ = Printf.fprintf oc "%s\n" (pr_input1 |> Pure_regexp_in.string_of_pure_regexp_in)

let _ = Printf.fprintf oc "%s\n" (pr_input2 |> Pure_regexp_in.string_of_pure_regexp_in)


let _ = close_out oc





(* let pr_output = Transformer.transform (Parser.Charset.explode alphabet_set) pr_input

let _ = print_endline (Pure_regexp_out.unparse pr_output) *)
