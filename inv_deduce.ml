module ST = Tree.SyntaxTree
module FT = Frac
module SM = Sparse_matrix
module FM = Fourier_motzkin.Elimination
module Solver = Simplex.SimplexSolver
module Printer = Print

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
    print_string "Start a deduction from exprs to obj : \n";
    let thres = FT.neg (SM.get_elt_row obj 0) in
    let obj' = SM.copy obj in
    (* remove constant in obj *)
    SM.remove obj' 0;
    (* the min value is the negation of max *)
    let min = FT.neg (Solver.inv_deduce (Solver.max obj') exprs) in
    (* if min >= thres then this expression is included in
        * former one *)
    match FT.get_sign FT.(min - thres) with
    | FT.Pos| FT.Null -> 
        print_string "Finished a deduction from exprs to obj\n";
        Printer.dividing_line Printer.length_defaut '-';true
    | _ -> false

(* inv to inv deductioin *)
let inv_to_inv inv inv' vars =
    (* print infos before transformation *)
    print_string "Procede inv deduction for : \n";
    print_string "Initial Inv : \n"; ST.print_inv inv;
    print_string "\n";
    print_string "Inv to deduce : \n"; ST.print_inv inv';
    print_char '\n';
    (* transform an invariant to standard form *)
    let inv_a = list_to_array inv in
    let exprs = Solver.trans inv_a vars in
    let aux res obj =
        if res then
            inv_to_obj exprs obj vars
        else raise DeductionFault
    in
    let res = List.fold_left aux true inv' in
    print_string "Finished a inv to inv deduction";
    Printer.dividing_line Printer.length_defaut '#'; res

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
                print_string "Inversible case : \n";
                let exprs' = list_to_array exprs in
                Solver.trans exprs' vars
        in
        print_string "Finished trans for inv\n\n";
        let aux res obj =
            if res then
                let obj' = SM.copy obj in
                let thres = FT.neg (SM.get_elt_row obj 0) in
                SM.remove obj' 0;
                let min = FT.neg (Solver.solve ((Solver.max obj'), exprs)) in
                match FT.get_sign FT.(min - thres) with
                | FT.Pos| FT.Null -> true
                | FT.Neg -> false
            else raise DeductionFault in
        let res = List.fold_left aux true inv' in
        print_string "Finished a deduction with an affectation !\n";
        Printer.dividing_line Printer.length_defaut '#';
        res
    | ST.Condit (e, b1, b2) ->
        print_string "Start a condition section : \n";
        (* get the start and end invariant in block *)
        let inv2, inv3 = get_start_end b1 in
        (* verify if post-condition *)
        print_string "Verification of if post-condition deduction : \n";
        inv_to_inv (e::inv) inv2 vars &&
        (print_string "Verification of else post-condition deduction : \n";
        inv_to_inv inv3 inv' vars )&&
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
            if not (inv_deduce inv instr inv' vars) then
                raise DeductionFault
            else true
        | (_inv, _)::tl' ->
            if inv_deduce inv instr _inv vars then
                block_deduce (ST.Node (tl, inv')) vars
            else
                raise DeductionFault

let deducer (prog : ST.prog) =
    match prog with
    | (vars, len, block) ->
        block_deduce block vars
