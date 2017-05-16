module FT = Frac

type e = FT.t

module IntHash =
struct
    type t = int
    let equal i j = i=j
    let hash i = i land max_int
end

module IntHashtbl = Hashtbl.Make(IntHash)

type t = e IntHashtbl.t

type m = t list

exception DivideZero

let get_elt_row r i =
    try
        IntHashtbl.find r i
    with Not_found -> FT.zero

let sub r1 r2 =
    let aux row i elt =
        let e = get_elt_row row i in
        IntHashtbl.replace row i (FT.sub e elt)
    in IntHashtbl.iter (aux r1) r2

let add r1 r2 =
    let aux row i elt =
        let e = get_elt_row row i in
        IntHashtbl.replace row i (FT.add e elt)
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
    IntHashtbl.add r i elt