(*///////////////////////////PART 1////////////////////////////*)
(*////////////////////////An even function//////////////////////*)

let even (xs: int) = if xs mod 2 = 0 then true else false

(*//////////////////////Euclid's GCD algorithm//////////////////////*)

let rec euclid (a:int) (b:int):int = 
    if a = b then a
    else if a < b then euclid a (b-a)
    else euclid (a-b) b

(*///////////////////Simplifying fractions//////////////////////////*)

let frac_simplify ((a:int),(b:int)):(int*int) = 
    let gcd = euclid a b 
    in
    (a/gcd,b/gcd)  

(*//////////////////Max in a list of integers////////////////////////*)

let rec max  (xs:int list):int =
    match xs with 
    |[] -> raise(Failure "Input list must not be empty")
    |x1::[] -> x1
    |x1::rest -> if x1 > max(rest) then x1 else max(rest)
    
(*///////////////////////Taking elements from a list///////////////////*)

let rec take number mylist = 
    match mylist with
    |[]-> []
    |x::rest ->
	let size a = List.fold_left(fun count _ -> count +1) 0 a 
		in 
		if number < 0 || number = 0 then []
        else if number = 1 then [x]
   		else if number > size mylist || number = size mylist then mylist
        else x :: take (number-1) rest 
