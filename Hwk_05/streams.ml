(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee

let rec from n = 
  print_endline ("step " ^ string_of_int n) ; 
  Cons ( n, 
         delay (fun () -> from (n+1) )
       )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

(* Exercise: Define a stream named ``ones`` of type int stream 
   in which all values are ``1``. *) 
let ones: int stream =
  let rec ones_h () =
    Cons (1, delay ones_h)
  in ones_h ()


(* write some common list processing functions *)

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

let even x = x mod 2 = 0

let all_even = filter even nats

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

let all_evens_v2 = map (fun x -> x * 2) nats

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))

let all_evens_v3 = zip (+) nats nats

(* Computing factorials

   nats       = 1   2   3   4    5     6 
                 \   \   \   \    \     \
                  *   *   *   *    *     *
                   \   \   \   \    \     \
   factorials = 1-*-1-*-2-*-6-*-24-*-120-*-720

   We can define factorials recursively.  Each element in the stream
   is the product of then "next" natural number and the previous
   factorial.
 *)

let factorials : int stream =
  let rec factorials_from n =
      Cons (n, delay (fun () -> zip ( * ) nats (factorials_from n)))
  in factorials_from 1

(*********************** The code below is from Khoa Tran **************************)

(* 3.1 cubes_from function *)
let rec cubes_from (n : int) : int stream = 
    Cons (n*n*n , delay(fun () -> cubes_from (n+1)))

(* zeros stream *)   
let zeros = 
    let rec zero_h () =
    Cons (0, delay zero_h)
    in zero_h ()

(* 3.2 cubes_from_zip function *)
let rec cubes_from_zip (n : int) : int stream = 
    let zip_stream = zip (+) zeros nats
    in
    match zip_stream with
    | Cons (h, tl) -> if (n = h + (n-1))
                      then Cons (n*n*n , delay (fun () -> cubes_from_zip (n+1))) 
                      else raise(Failure "this should not happen")

(* natural from 0 stream *)
let nats_0 = from 0

(* 3.3 cube_from_map function *)
let rec cubes_from_map (n : int ) : int stream = 
    let map_stream = map (fun x-> x+1) nats_0
    in
    match map_stream with
    | Cons(h, tl) -> if ( n = h + (n-1))
                     then Cons (n*n*n , delay (fun () -> cubes_from_zip (n+1))) 
                     else raise(Failure "this should not happen")

(* 3.4 drop function *)
let rec drop (n : int) (s : 'a stream) : 'a stream =
    match n,s with
    | 0, _ -> s
    | n, Cons (h,tl) -> drop (n-1) (demand tl)

(* 3.5 drop_until function *)
let rec drop_until (f : 'a -> bool) (s : 'a stream) : 'a stream = 
    match s with 
    | Cons (h,tl) -> 
            match f h with
            | true -> Cons (h,tl)
            | false -> drop_until f (demand tl)

(* 3.6 foldr , and_fold , and sum_positive_prefix functions *)
(* foldr function *)
(* my fold takes two arguments. One is a function that has type
 * ( 'a -> 'b lazee -> 'b), and the other is a 'a stream. 
 * The foldr will delay all the tail part of stream and apply 
 * function to the head of stream. This way, it doesn't to evaluate 
 * the whole stream.*)
let rec foldr (f : 'a -> 'b lazee -> 'b) (s : 'a stream) : 'b = 
    match s with
    | Cons (h,tl) -> f h (delay (fun () -> foldr f (demand tl)))

(* and_fold function *)
let and_fold (b : bool stream) : bool = 
    let and_fun =  
        fun (x1 : 'b) (x2: 'b lazee) ->
            if x1 then (x1 && (demand x2)) else false
    in 
     foldr and_fun b

(* sum_positive_prefix function *)
let rec sum_positive_prefix (i : int stream) : int = 
    let plus_fun = 
        fun (x1 : 'a) (x2 : 'a lazee) ->
            if x1 >= 0 then ((+) x1 (demand x2)) else 0
    in
    foldr plus_fun i

(* Test Data For foldr *)
let ns : int stream = zip ( - ) (from 1000) (cubes_from 1)
let are_positive ns = map (fun n -> n > 0) ns
let ns_positive : bool stream = are_positive ns

(* 3.7 The Sieve of Eratosthenes *)
let sift (n : int) (s : int stream) : int stream = 
    let multiple_of_n (x : int) (y : int) : bool =
                if y mod x = 0 then true else false
    in
    let not_n (f : 'a -> bool) (n : 'a) : bool = not (f n)
    in
    let func = not_n (multiple_of_n n)
    in
    filter func s

let rec sieve (s : int stream) : int stream = 
    match s with
    | Cons (h, tl) -> 
           Cons(h, delay(fun () -> sieve (sift h (demand tl))))

let primes = sieve (from 2)
































