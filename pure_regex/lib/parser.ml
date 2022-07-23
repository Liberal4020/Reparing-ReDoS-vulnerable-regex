(* from https://github.com/ocaml/ocaml/blob/trunk/otherlibs/str/str.ml *)

(** Representation of character sets **)

module Charset =
  struct
    type t = bytes (* of length 32 *)

    let empty = Bytes.make 32 '\000'
    let full = Bytes.make 32 '\255'

    let make_empty () = Bytes.make 32 '\000'

    let add s c =
      let i = Char.code c in
      Bytes.set s (i lsr 3)
                (Char.chr (Char.code (Bytes.get s (i lsr 3))
                           lor (1 lsl (i land 7))))

    let add_range s c1 c2 =
      for i = Char.code c1 to Char.code c2 do add s (Char.chr i) done

    let of_list cs =
      let s = make_empty () in List.iter (fun c -> add s c) cs; s

    let singleton c =
      let s = make_empty () in add s c; s

    let range c1 c2 =
      let s = make_empty () in add_range s c1 c2; s

    let complement s =
      let r = Bytes.create 32 in
      for i = 0 to 31 do
        Bytes.set r i (Char.chr(Char.code (Bytes.get s i) lxor 0xFF))
      done;
      r

    let union s1 s2 =
      let r = Bytes.create 32 in
      for i = 0 to 31 do
        Bytes.set r i (Char.chr(Char.code (Bytes.get s1 i)
                       lor Char.code (Bytes.get s2 i)))
      done;
      r

    let disjoint s1 s2 =
      try
        for i = 0 to 31 do
          if Char.code (Bytes.get s1 i) land Char.code (Bytes.get s2 i)
             <> 0
          then raise Exit
        done;
        true
      with Exit ->
        false

    let iter fn s =
      for i = 0 to 31 do
        let c = Char.code (Bytes.get s i) in
        if c <> 0 then
          for j = 0 to 7 do
            if c land (1 lsl j) <> 0 then fn (Char.chr ((i lsl 3) + j))
          done
      done

    let expand s =
      let r = Bytes.make 256 '\000' in
      iter (fun c -> Bytes.set r (Char.code c) '\001') s;
      r

    let fold_case s =
      (let r = make_empty() in
       iter (fun c -> add r (Char.lowercase c); add r (Char.uppercase c)) s;
       r)[@ocaml.warning "-3"]

    let mem s c =
      let code = Char.code c in
      let i = code / 8 in
      let j = code mod 8 in
      (Char.code (Bytes.get s i) land (1 lsl j)) <> 0
    let minus s1 s2 = complement (union (complement s1) s2)
    let explode s =
      let l = ref [] in
      iter (fun c -> l := c :: !l) s;
      List.rev !l
  end

(** Abstract syntax tree for regular expressions *)

type re_syntax =
    Char of char
  | String of string
  | CharClass of Charset.t * bool  (* true = complemented, false = normal *)
  | Seq of re_syntax list
  | Alt of re_syntax * re_syntax
  | Star of re_syntax
  | Plus of re_syntax
  | Option of re_syntax
  | Group of int * re_syntax
  | Refgroup of int
  | Bol
  | Eol
  | Wordboundary

(* Efficient buffering of sequences *)

module SeqBuffer = struct

  type t = { sb_chars: Buffer.t; mutable sb_next: re_syntax list }

  let create() = { sb_chars = Buffer.create 16; sb_next = [] }

  let flush buf =
    let s = Buffer.contents buf.sb_chars in
    Buffer.clear buf.sb_chars;
    match String.length s with
      0 -> ()
    | 1 -> buf.sb_next <- Char s.[0] :: buf.sb_next
    | _ -> buf.sb_next <- String s :: buf.sb_next

  let add buf re =
    match re with
      Char c -> Buffer.add_char buf.sb_chars c
    | _ -> flush buf; buf.sb_next <- re :: buf.sb_next

  let extract buf =
    flush buf; Seq(List.rev buf.sb_next)

end

(* The character class corresponding to `.' *)

let dotclass = Charset.complement (Charset.singleton '\n')

(* Parse a regular expression *)

let parse s =
  let len = String.length s in
  let group_counter = ref 1 in

  let rec regexp0 i =
    let (r, j) = regexp1 i in
    regexp0cont r j
  and regexp0cont r1 i =
    if i + 2 <= len && s.[i] = '\\' && s.[i+1] = '|' then
      let (r2, j) = regexp1 (i+2) in
      regexp0cont (Alt(r1, r2)) j
    else
      (r1, i)
  and regexp1 i =
    regexp1cont (SeqBuffer.create()) i
  and regexp1cont sb i =
    if i >= len
    || i + 2 <= len && s.[i] = '\\' && (let c = s.[i+1] in c = '|' || c = ')')
    then
      (SeqBuffer.extract sb, i)
    else
      let (r, j) = regexp2 i in
      SeqBuffer.add sb r;
      regexp1cont sb j
  and regexp2 i =
    let (r, j) = regexp3 i in
    regexp2cont r j
  and regexp2cont r i =
    if i >= len then (r, i) else
      match s.[i] with
        '?' -> regexp2cont (Option r) (i+1)
      | '*' -> regexp2cont (Star r) (i+1)
      | '+' -> regexp2cont (Plus r) (i+1)
      |  _  -> (r, i)
  and regexp3 i =
    match s.[i] with
      '\\' -> regexpbackslash (i+1)
    | '['  -> let (c, compl, j) = regexpclass0 (i+1) in
              (CharClass(c, compl), j)
    | '^'  -> (Bol, i+1)
    | '$'  -> (Eol, i+1)
    | '.'  -> (CharClass(dotclass, false), i+1)
    | c    -> (Char c, i+1)
  and regexpbackslash i =
    if i >= len then (Char '\\', i) else
      match s.[i] with
        '|' | ')' ->
          assert false
      | '(' ->
          let group_no = !group_counter in
          incr group_counter;
          let (r, j) = regexp0 (i+1) in
          if j + 1 < len && s.[j] = '\\' && s.[j+1] = ')' then
            (Group(group_no, r), j + 2)
          else
            failwith "\\( group not closed by \\)"
      | '1' .. '9' as c ->
          (Refgroup(Char.code c - 48), i + 1)
      | 'b' ->
          (Wordboundary, i + 1)
      | c ->
          (Char c, i + 1)
  and regexpclass0 i =
    if i < len && s.[i] = '^'
    then let (c, j) = regexpclass1 (i+1) in (c, true, j)
    else let (c, j) = regexpclass1 i in (c, false, j)
  and regexpclass1 i =
    let c = Charset.make_empty() in
    let j = regexpclass2 c i i in
    (c, j)
  and regexpclass2 c start i =
    if i >= len then failwith "[ class not closed by ]";
    if s.[i] = ']' && i > start then i+1 else begin
      let c1 = s.[i] in
      if i+2 < len && s.[i+1] = '-' && s.[i+2] <> ']' then begin
        let c2 = s.[i+2] in
        Charset.add_range c c1 c2;
        regexpclass2 c start (i+3)
      end else begin
        Charset.add c c1;
        regexpclass2 c start (i+1)
      end
    end in

  let (r, j) = regexp0 0 in
  if j = len then r else failwith "spurious \\) in regular expression"
