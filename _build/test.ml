module ST = Tree.SyntaxTree
module FM = Fourier_motzkin.Elimination
module FT = Frac

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

List.map (fun a -> FT.print_array a) (fst inv');;
naive