open Intervals

module Rational_comparable : (Comparable with type t = int*int) = struct
    type t = int * int
    let compare t1 t2 =
        match t1 , t2 with
        | (a,b) , (c,d) -> if ((float_of_int a /. float_of_int b) <= (float_of_int c /. float_of_int d)) then -1 else 1

    let to_string (a,b) = 
            let rec euclid (a1:int) (b1:int):int = 
                if a1 = b1 then a1
                else if a1 < b1 then euclid a1 (b1-a1)
                else euclid (a1-b1) b1
            in    
            let frac_simplify ((a2:int),(b2:int)):(int*int) = 
                let gcd = 
                    euclid a2 b2 
                    in
                    (a2/gcd,b2/gcd)
   in
   let (x,y) = frac_simplify (a,b)
   in
   string_of_int x ^ "/" ^ string_of_int y
end

module Rational_interval = Make_interval(Rational_comparable)

