module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac
module Solver = Simplex.SimplexSolver
module SM = Sparse_matrix
module Deducer = Inv_deduce

let q = ST.read_prog "prog.txt"
let st = ST.cons_prog q

let res = Deducer.deducer st

(*
let obj = SM.create 10;;
let exprs = Array.make 3 SM.empty;;
exprs.(0)<-SM.create 10;
exprs.(1)<-SM.create 10;
exprs.(2)<-SM.create 10;

SM.add_element obj 1 (-2,1); SM.add_element obj 2 (-1, 1);
SM.add_element exprs.(0) 1 (3, 1); SM.add_element exprs.(0) 2 (1, 1);
SM.add_element exprs.(0) 3 (1, 1); SM.add_element exprs.(0) 0 (6, 1);
SM.add_element exprs.(1) 1 (1, 1); SM.add_element exprs.(1) 2 (-1, 1);
SM.add_element exprs.(1) 4 (1, 1); SM.add_element exprs.(1) 0 (2, 1);
SM.add_element exprs.(2) 2 (1, 1); SM.add_element exprs.(2) 0 (3, 1);
SM.add_element exprs.(2) 5 (1, 1);
let tab = obj, exprs in

Solver.solve tab
*)