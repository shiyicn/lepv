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

let obj = SM.create 10;;
let exprs = Array.make 2 SM.empty;;
exprs.(0)<-SM.create 10;
exprs.(1)<-SM.create 10;

SM.add_element obj 1 (-1,1); SM.add_element obj 2 (-1, 1);
SM.add_element exprs.(0) 1 (2, 1); SM.add_element exprs.(0) 2 (1, 1);
SM.add_element exprs.(0) 3 (1, 1); SM.add_element exprs.(0) 0 (4, 1);
SM.add_element exprs.(1) 1 (1, 1); SM.add_element exprs.(1) 2 (2, 1);
SM.add_element exprs.(1) 4 (1, 1); SM.add_element exprs.(1) 0 (3, 1);
let tab = obj, exprs in

Solver.solve tab;;