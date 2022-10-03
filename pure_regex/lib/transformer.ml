(* pure_regexp -- Thompson's construction --> NFA -- powerset construction --> DFA -- state elimination --> pure_regexp *)
let transform alphabet_list r = r
    |> Thompson.thompson
    |> Powerset.powerset
    |> Dfa.minimize alphabet_list
    |> Gnfa.of_dfa alphabet_list
    |> State_elimination.state_elimination
