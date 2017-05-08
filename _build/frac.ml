type t = int * int

let convert (a : int array) = 
    Array.map (fun a -> (a, 1)) a

let tostring a = (string_of_int (fst a))^"/"^(string_of_int (snd a))

let print_array (a : t array) =
    ignore(Array.map 
    (fun a -> print_string ((tostring a)^"\t")) a);
    print_string "\n"

