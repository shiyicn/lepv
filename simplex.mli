module SimplexSolver =
sig
    type t
		
		val convert : 

    (*pivot operation
    * t : canoical table with form
            object : max z = sum (a_0i * x_i)
            rows : sum_k (a_ki * x_i) >= 0
    * i : pivot row index
    * j : entering variable index
    *)
    val pivot : t -> int -> int -> unit

end