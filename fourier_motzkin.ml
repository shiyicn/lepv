module ST = Tree.SyntaxTree
module FT = Frac

module Elimination = 
struct

  type set = {leq : ST.inv; geq: ST.inv; other : ST.inv}
  let empty = {leq = []; geq = []; other = []}

  exception NoAffEliminate

  (*check if an affectation is inversible*)
  let is_inversible (instr : ST.instr) = 
    match instr with
    | ST.Aff (i, expr) ->
      if FT.get_sign expr.(i) = FT.Null then false else true
    (* no affectation in Loop or Condit *)
    | _ -> raise NoAffEliminate

  (*eliminate xi from invariant set inv and
    return a new equivalent invariant set*)
  let eliminate (instr : ST.instr) (inv : ST.inv) : (ST.inv * ST.instr) =
    match instr with
    | ST.Aff (i, e) ->

      if is_inversible instr then
        let t = FT.neg e.(i) in
        let e' = e.(i) <- (-1, 1); Array.map (fun a -> FT.div a t) e in
        (* eliminate ancient xi with xi' in the affectation *)
        let aux i e a = 
          let e' = Array.map (FT.times a.(i)) e in
          a.(i)<-FT.zero; Array.map2 FT.add a e'
        in (List.map (aux i e') inv), instr
      else
        (* leq stores expr <= xi; geq stores expr >= xi; other stores expr in
           which there's no xi *)
        let aux i s expr =
          let e = expr.(i) in
          let sign = FT.get_sign e in
          match sign with
          | FT.Null -> {leq = s.leq; geq = s.geq; other = expr::s.other}
          | _ -> 
            let expr' = (Array.map (fun x -> FT.div x (FT.neg e)) expr) in
            expr'.(i) <- FT.zero; 
            if sign = FT.Neg then
              {leq = s.leq; geq = expr'::s.geq; other = s.other}
            else
              {leq = expr'::s.leq; geq = s.geq; other = s.other}
        in

        let es = List.fold_left (aux i) empty inv in

        (* generate |leq| * |geq| new invariants *)
        let aux leq l e =
          List.fold_left (fun a b -> (Array.map2 FT.sub e b)::a) l leq in
        let l' = List.fold_left (aux es.leq) [] es.geq in
        (l'@es.other), instr

    |_ -> raise NoAffEliminate
end