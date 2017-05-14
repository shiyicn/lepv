module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac
module Solver = Simplex.SimplexSolver
(*
let q = ST.read_prog "prog.txt"
let st = ST.cons_prog q

let t = match st with
| (_, _, t) -> t

exception Test

let inv' = 
    match t with
    | ST.Node (hd::tl, inv) ->
        FM.eliminate (snd hd) (fst hd)
    | _ -> raise Test;;
*)

let tab = Array.make 3 (Array.make 3 (2, 1));;
let a = Solver.trans tab;;

Array.map (FT.print_array) tab;
print_string "\n\n";
Array.map (FT.print_array) a;;

(*List.map (fun a -> FT.print_array a) (fst inv');;*)