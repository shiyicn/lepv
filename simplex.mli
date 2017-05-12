module type SimplexSolver =
sig
    (* linear program type *)
    type t
		
    (* transform a linear program to standard form *)
		val trans : t -> t

    (* pivot operation *)
    val pivot : t -> int -> int -> unit

    (* find pivot *)
    val find_pivot : t -> int -> int

    (* get basic solution *)
    val basic_solution : t -> int array -> int

end