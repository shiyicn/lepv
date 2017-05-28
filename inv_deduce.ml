module ST = Tree.SyntaxTree
module FT = Frac
module SM = Sparse_matrix
module FM = Fourier_motzkin.Elimination
module Solver = Simplex.SimplexSolver

exception MalFormedBlock
exception EmptyBlock
exception DeductionFault

(* add an expression to a block *)
let add_expr b e =
    match b with
    | ST.Empty -> ST.Empty
    | ST.Node ([], instr) -> raise MalFormedBlock
    | ST.Node ((inv, instr)::tl, inv') -> 
        ST.Node ((e::inv, instr)::tl, inv')

(* reverse an expression *)
let reverse_expr e =
    let e' = SM.copy e in
    SM.div_const e' (-1, 1); e'

(* convert a list to an array *)
let list_to_array (l : 'a list) =
    match l with
    | [] -> [||]
    | hd::_ -> 
        let a = Array.make (List.length l) hd in
        let aux i elt = a.(i) <- elt
        in List.iteri aux l; a

(* single inv to objective *)
let inv_to_obj exprs obj vars= 
    (* remove constant in the objective function *)
    let thres = SM.get_elt_row obj 0 in
    let obj' = SM.copy obj in
    (* remove constant in obj *)
    SM.remove obj' 0;
    let min = (Solver.inv_deduce (Solver.max obj') exprs vars) in
    (* if min >= thres then this expression is included in
        * former one *)
    match FT.get_sign FT.(min - thres) with
    | FT.Pos| FT.Null -> true
    | _ -> false

(* inv to inv deductioin *)
let inv_to_inv inv inv' vars =
    (* transform an invariant to standard form *)
    let inv_a = list_to_array inv in
    let exprs = Solver.trans inv_a vars in
    let aux res obj =
        if res then
            inv_to_obj exprs obj vars
        else raise DeductionFault
    in 
    List.fold_left aux true inv'

let get_start_end block =
    match block with
    | ST.Empty -> raise EmptyBlock
    | ST.Node ([], inv') -> [], inv'
    | ST.Node ((inv0, instr)::tl, inv') -> inv0, inv'

let rec inv_deduce (inv : ST.inv) (instr : ST.instr) (inv' : ST.inv) vars =
    match instr with
    | ST.Aff (i, e) ->
        (* print infos before transformation *)
        print_string "Procede inv deduction for : \n";
        print_string "Initial Inv : \n"; ST.print_inv inv;
        Printf.printf "Affectation form : \t%d = " i; ST.print_expr e;
        print_char '\n';
        print_string "Inv to deduce : \n"; ST.print_inv inv';
        print_char '\n';
        (* procede fourier motzkin elimination *)
        let inv_t = FM.eliminate instr inv in
        ST.print_inv (fst inv_t);
        (* perform transformation for inv_t *)
        let exprs = 
            match inv_t with
            | (exprs, FM.Some ins) ->
                print_string "Non-inversible case : \n";
                let exprs' = list_to_array exprs in
                let exprs_t = Solver.trans exprs' vars in
                (* perform transform for affectation and  *)
                Array.append exprs_t [|(Solver.trans_affectation ins)|]
            | (exprs, FM.None) ->
                let exprs' = list_to_array exprs in
                Solver.trans exprs' vars
        in
        print_string "Finish trans for inv\n\n";
        let aux res obj =
            if res then
                let obj' = SM.copy obj in
                let thres = SM.get_elt_row obj 0 in
                SM.remove obj' 0;
                let min = Solver.solve (obj', exprs) in
                match FT.get_sign FT.(min - thres) with
                | FT.Pos| FT.Null -> true
                | FT.Neg -> false
            else raise DeductionFault in
        List.fold_left aux true inv'
    | ST.Condit (e, b1, b2) ->
        (* get the start and end invariant in block *)
        let inv2, inv3 = get_start_end b1 in
        (* verify if post-condition *)
        inv_to_inv (e::inv) inv2 vars &&
        inv_to_inv inv3 inv' vars &&
        (* precede block deduction *)
        block_deduce b1 vars &&
        let inv2', inv3' = get_start_end b2 in
        (* verify else post-condition *)
        inv_to_inv ((reverse_expr e)::inv) inv2' vars &&
        inv_to_inv inv3' inv' vars &&
        block_deduce b2 vars
    | ST.While (e, b) -> 
        let inv2, inv3 = get_start_end b in
        inv_to_inv (e::inv) inv2 vars &&
        inv_to_inv ((reverse_expr e)::inv) inv' vars &&
        inv_to_inv (e::inv3) inv2 vars &&
        inv_to_inv ((reverse_expr e)::inv3) inv' vars &&
        (* procede block deduction *)
        block_deduce b vars

and block_deduce block vars = 
    match block with
    | ST.Empty -> true
    | ST.Node ([], _) -> raise MalFormedBlock
    | ST.Node ((inv, instr)::tl, inv') -> 
        match tl with
        | [] ->
            inv_deduce inv instr inv' vars
        | (_inv, _)::tl' ->
            if inv_deduce inv instr _inv vars then
                block_deduce (ST.Node (tl, inv')) vars
            else
                raise DeductionFault

let deducer (prog : ST.prog) =
    match prog with
    | (vars, len, block) ->
        block_deduce block vars
