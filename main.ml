module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac
module Solver = Simplex.SimplexSolver
module SM = Sparse_matrix

let q = ST.read_prog "prog.txt"
let st = ST.cons_prog q

let t = match st with
| (_, _, t) -> t
