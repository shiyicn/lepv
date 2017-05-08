module ST = Tree.SyntaxTree
(*
module SL = Simplex.Simplex
module FM = Fourier_motzkin.FourierMotzkin
*)

let q = ST.read_prog "prog.txt"
let st = ST.cons_prog q
