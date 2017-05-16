module ST = Tree.SyntaxTree
module FT = Frac
module SM = Sparse_matrix

module SimplexSolver =
struct
    type t = SM.m

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
    (* define single transformation for an expression*)
    let ex_trans expr ex_in =
        let len = Array.length expr in
        let expr' = SM.create 10 in
        let sign_c = Frac.get_sign expr.(0) in (* const coefficient sign -- sign_c*)
        let aux i a = 
                if i = 0 then SM.add_element expr' 0 (FT.abs a) (* convert const to be non-negative*)
                else
                let sign' = FT.get_sign a in
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
    let trans tab = 
        let len = Array.length tab in
        Array.mapi (fun i a -> ex_trans a (2*len-1+i)) tab
    
    let max obj = (*transform objective to simplex standard*)
        let obj' = SM.create 10 in
        let aux i a =
                (* add constant to new obj *)
                if i = 0 then SM.add_element obj' a
                else
                match FT.get_sign a with
                (* processus 3 *)
                | FT.Neg| FT.Pos -> 
                        SM.add_element expr' (2*i-1) a;
                        SM.add_element expr' (2*i) (FT.neg a)
                | FT.Null -> () in
        Array.iteri aux obj
                        
    (*pivot operation
    * t : canoical table with form
            object : max z = sum (a_0i * x_i)
            rows : sum_k (a_ki * x_i) >= 0
    * i : pivot row index
    * j : entering variable index
    *)
    let pivot t i j = ()

    

end