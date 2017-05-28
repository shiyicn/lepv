module ST = Tree.SyntaxTree
module FT = Frac
module SM = Sparse_matrix

module Elimination = 
struct

  type set = {leq : ST.inv; geq: ST.inv; other : ST.inv}
  let empty = {leq = []; geq = []; other = []}

  exception NoAffEliminate

  type t =
    |Some of ST.instr | None

  (*check if an affectation is inversible*)
  let is_inversible (instr : ST.instr) = 
    match instr with
    | ST.Aff (i, expr) ->
      if FT.get_sign (SM.get_elt_row expr i) = FT.Null then false else true
    (* no affectation in Loop or Condit *)
    | _ -> raise NoAffEliminate

  (* eliminate xi from invariant set inv and
   * return a new equivalent invariant set, if
   * it's a form of affectation this function
   * returns inv_eliminated, and option form affectation.
   * This depends on the inversibility of the affectation.
   * On the other side, raise a non-affectation exception if
   * the instruction is not an affectation form.
   *)
  let eliminate (instr : ST.instr) (inv : ST.inv) : (ST.inv * t) =
    print_string "FM eliminates instr and inv.\n";
    match instr with
    | ST.Aff (i, e) ->

      if is_inversible instr then
        (* get the negation of variable i *)
        let t = FT.neg (SM.get_elt_row e i) in
        (* transform the row; replace variable i with (-1, 1)
         * divid every variable coefficient with t
         *)
        let () = SM.replace e i (-1, 1); SM.div_const e t in
        (* eliminate ancient xi with xi' in the affectation *)
        let aux i e a = 
          (* firstly make a copy of e to avoid changing original e *)
          let e' = SM.copy e in
          let () = SM.times_const e' (SM.get_elt_row a i) in
          SM.remove a i; SM.add a e' in
        (* trans every expression in inv *)
        ignore(List.iter (aux i e) inv); inv, None
      else
        (* leq stores expr <= xi; geq stores expr >= xi; other stores expr in
           which there's no xi *)
        let aux i s expr =
          let e = SM.get_elt_row expr i in
          let sign = FT.get_sign e in
          match sign with
          | FT.Null -> {leq = s.leq; geq = s.geq; other = expr::s.other}
          | _ -> 
            SM.div_const expr (FT.neg e); SM.remove expr i; 
            if sign = FT.Neg then
              {leq = s.leq; geq = expr::s.geq; other = s.other}
            else
              {leq = expr::s.leq; geq = s.geq; other = s.other}
        in

        let es = List.fold_left (aux i) empty inv in

        (* generate |leq| * |geq| new invariants *)
        let aux leq l e =
          List.fold_left
            (* make a copy of e, avoid destroying original e *)
            (fun a b -> let e' = SM.copy e in (SM.sub e' b); e'::a) 
            l leq in
        let l' = List.fold_left (aux es.leq) [] es.geq in
        (l'@es.other), Some instr

    |_ -> raise NoAffEliminate
end