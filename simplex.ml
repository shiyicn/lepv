module ST = Tree.SyntaxTree

module SimplexSolver =
struct
    type t = ST.expr array

    (*pivot operation
    * t : canoical table with form
            object : max z = sum (a_0i * x_i)
            rows : sum_k (a_ki * x_i) >= 0
    * i : pivot row index
    * j : entering variable index
    *)
    let pivot t i j = ()

end