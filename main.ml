
module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac
module Solver = Simplex.SimplexSolver
module SM = Sparse_matrix
module Deducer = Inv_deduce
module Printer = Print

let () = print_string "Type the txt file name: \n"

let file = read_line ()

let () =
    Printf.printf "Construct the program for: %s\n" file;
    Printer.dividing_line Printer.length_defaut '#'

(* Firstly construct a syntax tree *)
let q = ST.read_prog file
let st = ST.cons_prog q

let () =
    Printer.dividing_line Printer.length_defaut '#';
    print_string "\nConstructed program\n\n"

(* try to verify all invariants in this program
 * deducer stops when we cannot deduce from an
 * an invariant to another one
 *)
let () = 
try
    ignore(Deducer.deducer st); print_string "Correct annotation.\n"
with Deducer.DeductionFault -> print_string "Incorrect annotation.\n"