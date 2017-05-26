module ST = Tree.SyntaxTree
module FT = Frac
module SM = Sparse_matrix

module IntHash =
struct
  type t = int
  let equal i j = i=j
  let hash i=i land max_int
end

module IntHashtbl = Hashtbl.Make(IntHash)

type count = int IntHashtbl.t

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
   * other elements are stored in other index *)
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

  (* standardise expr array *)
  let trans tab htl= 
    (* find appropriate len for variables*)
    let len = ST.get_var_size htl  in
    Array.mapi (fun i a -> ex_trans a (2*len-1+i)) tab

  (* transform objective to simplex standard form *)
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

  exception FoundNegIndex

  (* pick the first negative element index *)
  let pick_neg (tab:t) = 
    match tab with
    | (obj, _ ) -> SM.find_neg obj

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

  (* pivot operation
   * t : canoical table with form
          object : max z = sum (a_0i * x_i)
          rows : sum_k (a_ki * x_i) >= 0
   * i : pivot row index
   * j : entering variable index
   One elimination of objective and constraint 
  *)
  let pivot p i j row_index row = 
    (* suppose that p (the pivot) is already normalised *)
    (* element in row with index j *)
    let e = SM.get_elt_row row j in
    if row_index = i || e = FT.zero then ()
    else
      let r' = SM.copy p in
      (* transform firstly r' to e*r' *)
      SM.times_const r' e; SM.sub row r'

  (*first use pivot function, 
  *then use pivot_exprs function, 
  *for an elimination of the exprs*)
  let pivot_exprs (tab: t) i j = 
    match tab with
    | (obj, exprs) ->
      (* firstly normalise exprs.(i) *)
      let e = SM.get_elt_row exprs.(i) j in
      SM.div_const exprs.(i) e;
      Array.iteri (pivot exprs.(i) i j) exprs;
      (* pivot operation for objective function *)
      pivot exprs.(i) i j (-1) obj

  exception NegCoeff
  exception LackConstraints of int
  exception NullElement

  (* get the current basic solution *)
  let is_solution (tab : t) = 
    let count = IntHashtbl.create 10 in
    match tab with 
    | (obj, exprs) ->
      (* auxiliary function to get all non-basic variables
       * index
       *)
      let aux expr =
        SM.iter 
          (fun i e -> 
             try
               let n = IntHashtbl.find count i in
               IntHashtbl.replace count i (n+1)
             with Not_found -> IntHashtbl.add count i 0)
          expr in
      (* iterate all elements in expressions : exprs and 
      * objective function : obj
      *)
      Array.iter aux exprs; aux obj;
      (*only preserve the basic variables*)
      let aux i e = 
        match FT.get_sign e with
        | FT.Neg -> raise NegCoeff
        | FT.Null -> raise NullElement
        | FT.Pos ->
          try
            match IntHashtbl.find count i with
            | 1 -> raise (LackConstraints i)
            | _ -> ()
          with Not_found -> raise (LackConstraints i) in
      try
        SM.iter aux obj; true
      with
      | NegCoeff -> Printf.printf "Negtive coefficient exists.\n"; false
      | NullElement -> Printf.printf "Null element should not appear in sparse row.\n"; false
      | LackConstraints i -> Printf.printf "Lack constraints for variable : %d" i;false

  let get_solution (tab : t) =
    match tab with
    | (obj, exprs) ->
      SM.get_elt_row obj 0
end