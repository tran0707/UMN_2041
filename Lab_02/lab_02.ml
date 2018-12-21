(* Name: Khoa Tran
 * Student ID: 5411431 
 * Assignment: LAB 2 *)
(* #1 Circle circumference, version 1 *)
let circle_circum_v1 radius = 2.0 *. 3.1415 *. radius;; 

(* #2 Circle circumference, version 2 *) 
let circle_circum_v2 radius =
	let pi = 3.1415
	in
	let do_math calculate =
	2.0 *. pi *. calculate 
	in
	do_math radius;;

(* #3 Product of a list of elements *)
let rec product xs = 
	match xs with
	|[]->1
	|x::rest_number -> x * product rest_number;;

(* #4 Sum of squared differences *)
let rec sum_sqrdiffs xs =
match xs with 
| x1::x2::[] -> (x1 - x2)*(x1 - x2)
| x1::x2::rest -> (x1 - x2) * (x1 - x2) + sum_sqrdiffs(x2::rest) ;;

(* #5 2D points and distance between them *)
let distance (x1,y1) (x2,y2) = 
    sqrt((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2));;

(* #6 Triangle perimeter *)
let triangle_perimeter (x1 , y1) (x2 , y2) (x3 , y3) =
    distance(x1 , y1)(x2 , y2)
    +. distance(x2 , y2)(x3 , y3)
    +. distance(x1 , y1)(x3 , y3) ;;

