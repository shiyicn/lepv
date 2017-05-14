module ST = Tree.SyntaxTree
module FT = Frac

module SimplexSolver =
struct
    type t = ST.expr array

    (* transform a linear program to standard form 
    * convert min object to max 
    * if a constraint is sum(a_i * x_i) <= b convert it to
            sum(a_i * x_i) + s = b where s >= 0
    * if a constraint is sum(a_i * x_i) >= b convert it to
            sum(a_i * x_i) - s = b where b >= 0
    * if some variables xj is unrestricted in sign, replace it everywhere with
            s''_j - s'_ j with s''_j, s'_j >= 0
    *)
    let trans tab = (*tansform expr to simplex standard*)
        let lenExpr = Array.length tab.(0) in               
        let funTab expr = 
                let newexpr = Array.make (2 * lenExpr) FT.zero in
                Array.iteri (fun i a ->match i with
                      | 0 -> newexpr.(0) <- FT.neg expr.(0)
                      | _ -> newexpr.(2 * i - 1) <- expr.(i); newexpr.(2 * i) <- FT.neg expr.(i)) expr
                     ;newexpr.(Array.length newexpr -1) <- (-1, 1)
                     ;newexpr
        in Array.map funTab tab

    (*pivot operation
    * t : canoical table with form
            object : max z = sum (a_0i * x_i)
            rows : sum_k (a_ki * x_i) >= 0
    * i : pivot row index
    * j : entering variable index
    *)
    let pivot t i j = ()

end