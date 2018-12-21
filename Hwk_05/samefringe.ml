(* This code below is from Nathan Ringo *)

type 'a bintree
  = Lf of 'a
  | Nd of 'a bintree * 'a bintree

let t1 = Nd(Nd(Lf(1), Lf(2)), Nd(Lf(3), Lf(4)))
and t2 = Nd(Lf(1), Nd(Lf(2), Nd(Lf(3), Lf(4))))
and t3 = Nd(Nd(Nd(Lf(1), Lf(2)), Lf(3)), Lf(4))
and t4 = Nd(Lf(4), Nd(Lf(3), Nd(Lf(2), Lf(1))))
and t5 = Nd(Nd(Nd(Lf(4), Lf(3)), Lf(2)), Lf(1))

(* Trees t1, t2, and t3 have the same fringe. *)
(* Trees t4 and t5 have the same fringe. *)

(* The Naive Solution *)
let rec append_strict l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: append_strict t l2

let rec equal_list_strict l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) -> x = y && equal_list_strict xs ys
  | _ -> false

let rec flatten_strict = function
  | Lf(x) -> [x]
  | Nd(l, r) -> append_strict (flatten_strict l) (flatten_strict r)

let eqleaves_strict t1 t2 =
  equal_list_strict (flatten_strict t1) (flatten_strict t2)

(* This code below was build upon the foundation of Professor Eric Van Wyk
 *  and Nathan Ringo *) 
 
(* The Lazy Solution *)
type 'a lazee = 'a hidden ref
and 'a hidden
	= Value of 'a
	| Thunk of (unit -> 'a);;

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")


type 'a lazy_list
  = Cons of 'a * 'a lazy_list lazee
  | Nil

let rec append_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : 'a lazy_list =
    match l1 with
    | Nil -> l2
    | Cons (h,t) -> Cons (h , delay(fun () -> append_lazy (demand t) l2) )

let rec equal_list_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : bool =
    match (l1,l2) with 
    | (Nil,Nil) -> true
    | (Cons(h1,t1) , Cons(h2,t2)) -> h1 = h2 && (equal_list_lazy (demand t1) (demand t2))
    | (_,_) -> false

let rec flatten_lazy (t: 'a bintree) : 'a lazy_list =
    match t with
    | Lf (x) -> Cons(x, delay(fun () -> Nil))
    | Nd(l,r) -> append_lazy (flatten_lazy l)(flatten_lazy r)

let eqleaves_lazy (t1: 'a bintree) (t2: 'a bintree) : bool =
    equal_list_lazy (flatten_lazy t1) (flatten_lazy t2)


(* The eqleaves_lazy use less space than eqleaves_strict because the
 * delay evaluation in append_lazy function that causes the flatten_lazy
 * function also delay to evaluate the tail of Cons. The eqleaves_lazy
 * function calls flatten_lazy twice which even save more space.
 * The eqleaves_strict,on the other hand, they evaluate all the tree 
 * without any kinds of delay and causes it to use way more space to
 * compare the leaves equality of the tree. To conlusion, the 
 * eqleaves_lazy use less space compare to eqleaves_strict. 
 *)








