module FT = Frac

module StringHash : Cs.Hashable with type t = string = 
struct
  type t = string
  let hash s = 
    let rec aux s i h len=
      if i = len then h
      else aux s (i+1) (h+int_of_char s.[i]) len
    in aux s 0 0 (String.length s)
  let equal s1 s2 = s1=s2
end

module Variables = 
struct
  (* the type of Variables *)
  type t

  module V = StringHash

  type var = V.t
  module VarHashtbl = Hashtbl.Make(V)
end

module SyntaxTree = 
struct
  module VarHashtbl = Hashtbl.Make(Variables.V)
  
  type var_set = int VarHashtbl.t

  (*
  Var : variables s "string"; As : assignment; 
  Op: operator; Pun : Punctuation : ";"; 
  Com : comparator : ">=, >, <=, <" ...
  *)
  type syntax =
    | Var| As| Op| Pun| Com| Int
    | Num| And| Cond| Loop | Blank
    | Comma| Curl| Curr| Parl| Parr

  type expr = FT.t array
  type inv = expr list

  type instr =
    | Aff of int * expr
    | Condit of expr * tree * tree
    | While of expr * tree
  and tree =
    | Empty
    | Node of (inv * instr) list * inv

  type prog = string VarHashtbl.t * int * tree

  type token = syntax * string

  exception IllegalCharacter

  let classer c =
    match c with
    | '=' -> As
    | ',' -> Comma
    | '{' -> Curl
    | '}' -> Curr
    | '(' -> Parl
    | ')' -> Parr
    | ';' -> Pun
    | '+'| '-'| '*'| '/' -> Op
    | '>'| '<' -> Com
    | '&' -> And
    | ' ' -> Blank
    | _ ->
      if c <= '9' && c >= '0' then Num
      else if (c <= 'z' && c >= 'a') || (c >= 'A' && c <= 'Z') then Var
      else (print_char c; raise IllegalCharacter)

  let identifier s = 
    match s with
    | "int" -> Int
    | "if"| "else" -> Cond
    | "while" -> Loop
    | _ -> Var

  (* tokenizer function *)
  let rec f st en len s q =
    let flag = classer s.[st] in
    let sub = String.sub s st (en - st) in
    if en = len then
      if flag = Blank then 
        q
      else
        (Queue.add (flag, sub) q; q)
    else
      let flag' = classer s.[en] in
      match (flag, flag') with
      | (Var, Num)| (Var, Var)| (Com, As) -> f st (en+1) len s q
      | (Var, _) -> Queue.add (identifier sub, sub) q; f en (en+1) len s q
      | (_, _) ->
        if flag = flag' then
          f st (en+1) len s q
        else
          (if flag = Blank then () else (Queue.add (flag, sub) q);
           f en (en+1) len s q)

  (*fold over every line of program txt*)
  let iofold (f : int -> int -> int -> string -> 'b -> 'b) (fname : string) (x : 'b) =
    let stream = open_in fname in
    try
      let rec aux (x : 'b) =
        try
          let s = input_line stream in
          aux (f 0 1 (String.length s) s x)
        with End_of_file -> x
      in let x = aux x in close_in stream; x
    with e -> close_in stream; raise e

  (* lexical analyser for annoted program
     Ex : prog_txt -> lexical token queue : syntax * string
  *)
  let read_prog fname = iofold f fname (Queue.create ())

  exception EmptyQueue
  exception ExpectSyntax of string

  (* build variables list
     Ex : "x0, x1, x2, ..., xn" -> a map from "xi" -> i
  *)
  let cons_var q htl= 
    if Queue.is_empty q then
      raise EmptyQueue
    else

      let rec aux q (i : int) htl = 
        let a = Queue.pop q in
        match a with
        | (Var, s) -> VarHashtbl.add htl s i;
          let a' = fst (Queue.pop q) in
          if a' = Comma then aux q (i+1) htl
          else if a' = Pun then q
          else raise (ExpectSyntax ", or ; is expected after a var in declaration")
        | _ -> raise (ExpectSyntax "Var is expected in declaration") in

      aux q 1 htl

  (*build expression
    Ex : "c + a0 * x0 + ... + an * xn }" -> [|c, a0, ..., an|]
  *)
  let cons_expr q l htl = 
    if Queue.is_empty q then raise EmptyQueue
    else if fst (Queue.peek q) = Curr then l
    else
      let ar = Array.make (VarHashtbl.length htl + 1) 0 in

      let rec aux q = 
        match Queue.pop q with
        | (Num, s) -> let i = aux q in (ar.(i) <- (int_of_string s)); i
        | (Op, s) -> let i = aux q in
          if s = "-" then (ar.(i) <- -ar.(i);0)
          else if s = "+" then 0 else if s = "*" then i else raise(ExpectSyntax "Operator is expected")
        | (Var, s) -> ignore(aux q); let i = VarHashtbl.find htl s in ar.(i) <- 1; i
        | (Curr, _)| (Pun, _)| (Com, _) -> 0
        | (_, s) -> raise (ExpectSyntax ("Invalid syntax : "^s^" in expr construction.\n"))
      in

      ignore(aux q);(*print basic expression info *)FT.print_array (FT.convert ar);
      (FT.convert ar)::l

  (*build invariant
    Ex : expr0 >= 0 & expr1 >= 0 & ... & exprl >= 0 -> expr list
  *)
  let rec cons_inv q htl l =
    if Queue.is_empty q then raise EmptyQueue
    else let e = Queue.pop q in
      match e with
      | (Curl, _)| (And, _)| (Parl, _) -> cons_inv q htl (cons_expr q l htl)
      | (Curr, _)| (Parr, _) -> l
      | (Num, _)| (Com, _) -> cons_inv q htl l
      | (_, s) -> raise (ExpectSyntax ("syntax : "^s^" is not valid in inv construction"))

  (*build an instruction : affectation, condition or while loop
    Ex : 
    Affectation : xj = expr -> Aff int * expr
    Condition : if(expr >= 0) block0 else block1 -> Condit expr0 * block0 * block1
    While loop : while(expr >= 0) block -> While expr * block
  *)
  let rec cons_intr q htl = 

    (*contruct a block surrounded by { }*)
    let aux q htl =
      let () = assert (fst (Queue.pop q) = Curl) in
      let block = cons_tree q htl in
      let () = assert (fst (Queue.pop q) = Curr) in
      block 
    in

    if Queue.is_empty q then raise EmptyQueue
    else let e = Queue.pop q in
      match e with
      | (Var, s) -> 
        if fst (Queue.pop q) = As then Aff (VarHashtbl.find htl s, List.hd (cons_expr q [] htl))
        else raise (ExpectSyntax "syntax : = is expected")
      | (Cond, _) -> 
        let ex = List.hd(cons_inv q htl []) in
        let block0 = aux q htl in
        let () = assert (fst (Queue.pop q) = Cond) in
        let block1 = aux q htl in
        Condit (ex, block0, block1)
      | (Loop, _) ->
        let ex = List.hd(cons_inv q htl []) in
        let block = aux q htl in
        While (ex, block)
      | (_, s) -> raise (ExpectSyntax ("Var, Cond, or Loop are expected, not : "^s^"\n"))

  (* build a block (a syntax tree)
     Ex : {inv0} : instr0; ... ; {invk} : instrk; {invk+1} ->
        (inv * intr) list * inv
  *)
  and cons_tree q htl =
    if Queue.is_empty q then raise EmptyQueue
    else
      let e = Queue.peek q in
      if fst e = Curl then
        let inv = cons_inv q htl [] in
        if (Queue.is_empty q || fst (Queue.peek q) = Curr) then Node ([], inv)
        else
          let instr = cons_intr q htl in
          let block' = cons_tree q htl in
          match block' with
          | Empty -> raise (ExpectSyntax "Empty tree is not desired.\n")
          | Node(l, inv') -> Node ((inv, instr)::l, inv')
      else raise (ExpectSyntax ("{ is expected for block construction, not : "^(snd e)^"\n"))

  (*
  Ex : a txt program contained 
  "int x0, ..., xn
  block"
  -> prog : VarHashtbl.t * int * tree
  *)
  let cons_prog (q : (syntax * string) Queue.t) =
    if Queue.is_empty q then raise EmptyQueue
    else
      let htl = VarHashtbl.create 10 in
      let e = Queue.pop q in
      match e with
      | (Int, _) -> ignore(cons_var q htl); (htl, (VarHashtbl.length htl), cons_tree q htl)
      | _ -> raise (ExpectSyntax "type int is expected")

  let get_var_size htl =
    VarHashtbl.length htl
end