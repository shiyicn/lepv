module type Ordered = sig
  type t
  (** compare x y : positive if x > y, 0 if x = y, negative if x < y *)
  val compare : t -> t -> int
end

module type Hashable = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

(* combination of Ordered and Hashable *)
module type Comparable = sig
  include Ordered
  include Hashable with type t := t
end