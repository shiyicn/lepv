module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac
module Solver = Simplex.SimplexSolver
module SM = Sparse_matrix
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

let row = SM.create 10;;

SM.add_element row 1 (4, 5);;
SM.add_element row 3 (7, 8);;

SM.times_const row (2, 4);;

SM.div_const row (2, 4);;

print_string (SM.to_string row);;

let row' = SM.create 10;;

SM.add_element row' 2 (3, 8);SM.add_element row' 11 (5, 8);;
print_string (SM.to_string row');;

SM.add row row';;

print_string (SM.to_string row);;



(*List.map (fun a -> FT.print_array a) (fst inv');;*)