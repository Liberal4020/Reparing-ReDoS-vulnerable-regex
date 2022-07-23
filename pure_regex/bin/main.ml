open Lib

let input = "(([^a])*)(([p])*)"

(* "<span[^>]*font-style:italic[^>]*>" *)

let _ = print_endline input

let alphabet_set = Parser.Charset.union (Parser.Charset.range 'a' 'z') (Parser.Charset.of_list ['<'; '>'; '-'; ':'])

let pr_input = input |> Util.replace "?:" "" |> Parser.parse |> Pure_regexp_in.of_regexp alphabet_set

let _= print_endline (pr_input |> Pure_regexp_in.string_of_pure_regexp_in)


(* let pr_output = Transformer.transform (Parser.Charset.explode alphabet_set) pr_input

let _ = print_endline (Pure_regexp_out.unparse pr_output) *)
