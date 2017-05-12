module ST = Tree.SyntaxTree

module SimplexSolver =
struct
    type t = ST.expr array

    let find_sign 

    (* transform a linear program to standard form 
    * convert min object to max 
    * if a constraint is sum(a_i * x_i) <= b convert it to
            sum(a_i * x_i) + s = b where s >= 0
    * if a constraint is sum(a_i * x_i) >= b convert it to
            sum(a_i * x_i) - s = b where b >= 0
    * if some variables xj is unrestricted in sign, replace it everywhere with
            s''_j - s'_ j with s''_j, s'_j >= 0
    *)
    let trans tab = 
        

    (*pivot operation
    * t : canoical table with form
            object : max z = sum (a_0i * x_i)
            rows : sum_k (a_ki * x_i) >= 0
    * i : pivot row index
    * j : entering variable index
    *)
    let pivot t i j = ()

end