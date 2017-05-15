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
        let f expr = 
                let expr' = Array.make (2 * lenExpr + 1) FT.zero in
                let aux i a = 
                match i with
                | 0 -> 
                if FT.get_sign a = FT.Neg then
                        begin
                        expr'.(0) <- FT.neg a; expr'.(Array.length expr' - 2) <- (1,1)
                        end
                else
                        begin
                        expr'.(0) <- a.(0);
                        expr'.(2 * lenExpr) <- (1, 1);
                        expr'.(Array.length expr' - 2) <- (-1, 1)
                        end
                | _ -> expr'.(2 * i - 1) <- a.(i); expr'.(2 * i) <- FT.neg a.(i) in
                Array.iteri aux expr; expr' in
        Array.map f tab
    
    (*
    let max objective = (*transform objective to simplex standard*)
              let maxexpr = Array.make(Array.length expr + 1) FT.zero in
                  let f i a =
                          match i with
                          | 0 -> maxexpr.(0) <- (1,1)
                          | _ -> matexpr.(i) <- FT.neg a.(i)  
                          in Array.iteri f objective
                 ;maxexpr
                *)

                        
    (*pivot operation
    * t : canoical table with form
            object : max z = sum (a_0i * x_i)
            rows : sum_k (a_ki * x_i) >= 0
    * i : pivot row index
    * j : entering variable index
    *)
    let pivot t i j = ()

end