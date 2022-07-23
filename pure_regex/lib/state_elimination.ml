open Gnfa
open Pure_regexp_out

let convert (GNFA(m, d)) =
  let d' (q1, q2) =
    let shift q = if q = Z.(~$0) then Z.(~$0) else Z.(q + ~$1) in
    let r123 =
      let r1 = d (shift q1, Z.(~$1)) in
      if r1 = Alter []
      then Alter[]
      else
        let r3 = d (Z.(~$1), shift q2) in
        if r3 = Alter []
        then Alter []
        else
          let r2 = d (Z.(~$1), Z.(~$1)) in
          if r2 = Alter[] || r2 = Concat []
          then Concat [r1; r3]
          else Concat [r1; Star r2; r3]
    and r4 = d (shift q1, shift q2) in
    if r4 = Alter [] then r123 else Alter [r123; r4]
  in GNFA(Z.(m - ~$1), d')

let rec state_elimination (GNFA(m, d)) = if m <= Z.(~$2) then d (Z.(~$0), Z.(~$1)) else state_elimination (convert (GNFA(m, d)))
