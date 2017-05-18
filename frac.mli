type t = int * int

type sign = | Null| Pos| Neg

val zero : t

val min_frac : t

val convert : int array -> t array

val to_string : t -> string

val print_array : t array -> unit

val sub : t -> t -> t

val add : t -> t -> t

val times : t -> t -> t

val neg : t -> t

val div : t -> t -> t

val get_sign : t -> sign

val com : t -> t -> sign

val abs : t -> t

val (+) : t -> t -> t

val (-) : t -> t -> t

val ( * ) : t -> t -> t

val (/) : t -> t -> t