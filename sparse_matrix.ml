module FT = Frac

module IntHash =
struct
  type t = int
  let equal i j = i=j
  let hash i = i land max_int
end

type e = FT.t

module IntHashtbl = Hashtbl.Make(IntHash)

type t = e IntHashtbl.t

type m = t array

exception DivideZero

let get_elt_row r i =
  try
    IntHashtbl.find r i
  with Not_found -> FT.zero

let sub r1 r2 =
  let aux row i elt =
    let e = get_elt_row row i in
    let sum = FT.sub e elt in
    if sum = FT.zero then
      IntHashtbl.remove row i
    else
      IntHashtbl.replace row i sum
  in IntHashtbl.iter (aux r1) r2

let add r1 r2 =
  let aux row i elt =
    let e = get_elt_row row i in
    let sum = FT.add e elt in
    if sum = FT.zero then
      IntHashtbl.remove row i
    else
      IntHashtbl.replace row i sum
  in IntHashtbl.iter (aux r1) r2

let times_const r elt =
  if elt = FT.zero then IntHashtbl.clear r
  else IntHashtbl.iter 
      (fun i e -> IntHashtbl.replace r i (FT.times e elt))
      r

let div_const r elt =
  if elt = FT.zero then raise DivideZero
  else IntHashtbl.iter 
      (fun i e -> IntHashtbl.replace r i (FT.div e elt))
      r

let to_string r =
  (IntHashtbl.fold 
     (fun i a b -> "("^(string_of_int i)^" , "^(FT.to_string a)^")\t"^b)
     r
     "")^"\n"

let create n = IntHashtbl.create n

let add_element r i (elt : e) =
  if elt = FT.zero then Printf.printf "Zero is ignored!\n"
  else IntHashtbl.add r i elt

exception FoundNegIndex of int
exception NullElementInSparseRow

let find_neg (r : t) =
  let index = ref (-1) in
  (try
     IntHashtbl.iter 
       (fun i a -> 
          match (FT.get_sign a) with
          | FT.Neg -> raise (FoundNegIndex i)
          | FT.Null -> raise NullElementInSparseRow
          | FT.Pos -> ()
       ) r
   with FoundNegIndex i -> index := i; Printf.printf "negative index found : %d\n" i);
  !index

let iter f (r : t) =
  IntHashtbl.iter f r

let fold_left f (matrix : m) a =
  Array.fold_left f a matrix

let replace r i elt =
  IntHashtbl.replace r i elt