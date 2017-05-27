
(* element type of matrix*)
type e = Frac.t

(* matrix row type t *)
type t

(* matrix type m *)
type m = t array

(* empty row *)
val empty : unit -> t

(* get element in a row *)
val get_elt_row : t -> int -> e

(* row1 minus row2 and get a new row *)
val sub : t -> t -> unit

(* row1 adds row2 and get a new row*)
val add : t -> t -> unit

(* a row t times a constant e -> e*t *)
val times_const : t -> e -> unit

(* a row t divides a constant e -> t/a *)
val div_const : t -> e -> unit

(* convert a row to string *)
val to_string : t -> string

(* create a row with initial capacity n *)
val create : int -> t

(* add an element with index i to a row *)
val add_element : t -> int -> e -> unit

(* find the first negative instance in row *)
val find_neg : t -> int

(* iterator for all elements in a row *)
val iter_row : (int -> e -> unit) -> t -> unit

(* fold over all elements in a row *)
val fold_row : (int -> e -> 'a -> 'a) -> t -> 'a -> 'a

(* iteri for a sparse matrix *)
val iteri : (int -> t -> unit) -> m -> unit

(* fold over all elements in a matrix *)
val fold_left : ('a -> t -> 'a) -> m -> 'a -> 'a

(* replace an element_index with another element *)
val replace : t -> int -> e -> unit

(* make a copy of a specific row *)
val copy : t -> t

(* convert a row to string *)
val row_to_string : t -> string

(* convert the sign of the element with index i in a row *)
val neg : t -> int -> unit

(* remove variable i *)
val remove : t -> int -> unit