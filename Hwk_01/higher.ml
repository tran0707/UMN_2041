(*PART 2*)

(*All evens*)

let all_evens lst = 
    List.filter(fun x -> (x mod 2) = 0) lst

(*Increment all*)

let increment_all mylist = 
    List.map(fun y -> y+1) mylist

(*Max of integer as a fold*)

let max_fold maxlist =                             
    List.fold_left(fun x y -> if x > y then x else y) (List.nth maxlist 0) (maxlist : int list)

(* Sum and prod in one pass*)

let sum_prod lst =
    ((List.fold_left(fun x y -> x + y) 0 lst),(List.fold_left(fun x y -> x*y) 1 lst))

(* Splitting up a list*) 

let split fs lst =
    let accum =([],[])
    in
    let f(pre,cur) x =
        if fs x 
        then (List.rev cur::pre, [])
        else (pre,x::cur)
    in
    let(ps,cs) = List.fold_left f accum lst
    in 
    List.rev(List.rev cs :: ps)
