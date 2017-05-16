type t = int * int

type sign = 
    | Null| Pos| Neg

let zero = (0, 1)

(* convert an integer array to fraction array *)
let convert (a : int array) = 
    Array.map (fun a -> (a, 1)) a

(* convert a fraction array to string *)
let to_string a = (string_of_int (fst a))^"/"^(string_of_int (snd a))

(* print a fraction array *)
let print_array (a : t array) =
    ignore(Array.map 
    (fun a -> print_string ((to_string a)^"\t")) a);
    print_string "\n"

(* a b -> greatest common denominator of a and b *)
let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)

(*f -> simplified f*)
let sim (f:t) =
    match f with
    | (n, d) ->
    let e = gcd (abs n) (abs d) in (n/e, d/e)

(*f1 f2 -> f1 - f2*)
let sub (f1:t) (f2:t) = 
    match (f1, f2) with
    | ((n1, d1),(n2, d2)) -> 
    assert (d1 > 0 && d2 > 0);
    sim ((n1*d2 - n2*d1), (d1*d2))

(*f1 f2 -> f1 + f2*)
let add (f1:t) (f2:t) = 
    match (f1, f2) with
    | ((n1, d1),(n2, d2)) -> 
    assert (d1 > 0 && d2 > 0);
    sim ((n1*d2 + n2*d1), (d1*d2))

(*f1 f2 -> f1 * f2*)
let times (f1:t) (f2:t) = 
    match (f1, f2) with
    | ((n1, d1), (n2, d2)) -> 
    assert (d1 > 0 && d2 > 0);
    sim (n1*n2, d1*d2)

let neg f =
    match f with
    | (n, d) ->
    assert (d > 0); (-n, d)

(*f1 f2 -> f1 / f2*)
let div (f1:t) (f2:t) = 
    match (f1, f2) with
    | ((n1, d1), (n2, d2)) -> 
    assert (d1 > 0 && d2 > 0 && (not (n2 = 0)));
    let res = sim (abs(n1*d2), abs(d1*n2)) in
    if n1 * n2 < 0 then neg res else res

let get_sign f = 
    match f with
    | (n, d) ->
    assert (d > 0);
    if n > 0 then Pos else if n < 0 then Neg else Null

let abs f =
    match f with
    | (n, d) -> assert (d > 0); (abs n, d)