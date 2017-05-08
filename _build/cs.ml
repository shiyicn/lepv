module type Ordered = sig
  type t
  (** compare x y : positive if x > y, 0 if x = y, negative if x < y *)
  val compare : t -> t -> int
end

module StringHash = 
struct
  type t = string
  let hash s = 
    let rec aux s i h len=
      if i = len then h
      else aux s (i+1) (h+int_of_char s.[i]) len
    in aux s 0 0 (String.length s)
  let equal s1 s2 = s1=s2
end
