(* ands function *)

let rec ands (b : bool list) : bool = 
    match b with
    | [] -> true
    | x :: xs ->
            if (x = false) 
            then false
            else x && ands xs

