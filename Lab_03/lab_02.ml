(* Name: Khoa Tran
 * Assignment: LAB 3
 * Group: Khoa Tran - Quoc Le - Timothy Druschke    
 *) 
(* #1 Circle circumference, version 1 *)
let circle_circum_v1 radius = 2.0 *. 3.1415 *. radius 

(* #2 Circle circumference, version 2 *) 
(*delete the middle for the unnecessory function *)
let circle_circum_v2 r =
	let pi = 3.1415
	in
	2.0 *. pi *. r

(* #3 Product of a list of elements *)
let rec product xs = 
	match xs with
	|[]-> 1
	|x::rest_number -> x * product rest_number

(* #4 Sum of squared differences *)
(*Add the _ for cases less than 2 element in list, raise failure *)
let rec sum_sqrdiffs xs =
match xs with 
| x1::x2::[] -> (x1 - x2)*(x1 - x2)
| x1::x2::rest -> (x1 - x2) * (x1 - x2) + sum_sqrdiffs(x2::rest)
|_ -> raise (Failure " sum_sqrtiffs need at least two input")

(* #5 2D points and distance between them *)
(*Changed the **2.0*)
let distance (x1,y1) (x2,y2) = 
    sqrt((x1 -. x2)**2.0 +. (y1 -. y2)**2.0)

(* #6 Triangle perimeter *)
(* Changed the variable (x1,y1) to v1,...*)
let triangle_perimeter v1 v2 v3 =
    (distance v1 v2) +. (distance v2 v3)+. (distance v3 v1)

