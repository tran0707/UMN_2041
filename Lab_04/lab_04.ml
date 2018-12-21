(* length *)

let length lst =
    List.fold_left(fun x _ -> x + 1) 0 lst

(* andf *)

let andf lst =
    List.fold_left(fun x y ->  x && y) true lst

(*orf*)

let orf lst =
    List.fold_left(fun x y -> x || y) false lst

(*is-element*)

let is_element a lst =
    List.fold_left(fun x y -> x||y=a) false lst

(*list reverse*)

let rev lst =
    List.fold_left(fun x y -> y::x) [] lst

(*ASCII sum*)

let ascii_sum lst =
    List.fold_left(fun x y -> x + Char.code y) 0 (lst : char list)

(* laboxski *)
let lebowsski lst =
    List.fold_left(fun x y -> if y = '.' then y::x else y)['d'; 'u'; 'd'; 'e']  lst
