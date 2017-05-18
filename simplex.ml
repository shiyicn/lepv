module ST = Tree.SyntaxTree
module FT = Frac
module SM = Sparse_matrix

module SimplexSolver =
struct
  type t = SM.t * SM.m

  (* expression standardisation
   * expr -> expr' with expr'.(0) as constant element
  *)
  (* transform a constraint to standard form 
   * 1 : if a constraint is sum(a_i * x_i) <= b convert it to
          sum(a_i * x_i) + s = b where s >= 0
   * 2 : if a constraint is sum(a_i * x_i) >= b convert it to
          sum(a_i * x_i) - s = b where s >= 0
   * 3 : if some variables xj is unrestricted in sign, replace it everywhere with
          s''_j - s'_ j with s''_j, s'_j >= 0
  *)
  (* tansform expr to simplex standard *)
  (* expr'.(0) : constant
  * other elements are stored in other index
  *)
  (* define single transformation for an expression*)
  let ex_trans expr ex_in =
    let expr' = SM.create 10 in
    let sign_c = Frac.get_sign expr.(0) in (* const coefficient sign -- sign_c*)
    let aux i a = 
      if i = 0 then SM.add_element expr' 0 (FT.abs a) (* convert const to be non-negative*)
      else
        match sign_c with
        (* processus 3 *)
        | FT.Neg -> 
          SM.add_element expr' (2*i-1) a;
          SM.add_element expr' (2*i) (FT.neg a)
        | FT.Pos -> 
          SM.add_element expr' (2*i-1) (FT.neg a);
          SM.add_element expr' (2*i) a
        | FT.Null -> () in
    Array.iteri aux expr;
    (* processus 2 and 1*)
    if sign_c = FT.Neg then SM.add_element expr' ex_in (1, 1)
    else SM.add_element expr' ex_in (-1, 1)
    ;expr'

  (* standardise expr array*)
  let trans tab htl= 
    (* find appropriate len for variables*)
    let len = ST.get_var_size htl  in
    Array.mapi (fun i a -> ex_trans a (2*len-1+i)) tab

  (*transform objective to simplex standard form*)
  let max obj =
    let obj' = SM.create 10 in
    let aux i a =
      (* add constant to new obj *)
      if i = 0 then SM.add_element obj' 0 a
      else
        match FT.get_sign a with
        (* processus 3 *)
        | FT.Neg| FT.Pos ->
          SM.add_element obj' (2*i-1) a;
          SM.add_element obj' (2*i) (FT.neg a)
        | FT.Null -> () in
      SM.iter aux obj; obj'
  
  (* construct a linear program according to
  * an objective and some expressions
  *)
  let cons_program obj exprs = 
    (max obj), (trans exprs)

  (*pivot operation
   * t : canoical table with form
          object : max z = sum (a_0i * x_i)
          rows : sum_k (a_ki * x_i) >= 0
   * i : pivot row index
   * j : entering variable index
  *)
  let pivot (tab : t) i j = ()

  exception FoundNegIndex

  (* pick the first negative element index *)
  let pick_neg (tab:t) = 
    match tab with
    | (obj, _ ) -> SM.find_neg obj
  
  (* get the current basic solution *)
  let get_basic_solution t = ()

  (* find new pivot index *)
  let find_pivot (tab : t) =
    let index = pick_neg tab in
    if index = -1 then -1
    else
      let aux a expr =
        let e_i = SM.get_elt_row expr index in
        if e_i = FT.zero then a
        else
          match a with
          | (ratio, i, ic) ->
            let const = SM.get_elt_row expr 0 in
            let ratio' = FT.(const / e_i) in
            match FT.get_sign FT.(ratio' - ratio) with
            | FT.Pos -> (ratio', ic, ic+1)
            | _ -> a
      in
      let (_, i, _) = SM.fold_left aux (snd tab) (FT.min_frac, -1, 0)
      in i
end