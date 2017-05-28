module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac
module Solver = Simplex.SimplexSolver
module SM = Sparse_matrix
module Deducer = Inv_deduce

(* Firstly construct a syntax tree *)
let q = ST.read_prog "prog.txt"
let st = ST.cons_prog q

let () = 
    print_string "\n#########################\nConstructed program\n\n"

(* try to verify all invariants in this program *)
let res = Deducer.deducer st