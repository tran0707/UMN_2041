open LazeeModules
open StreamModules

module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) :
                (Hwk5Sig with type 'a stream = 'a S.t) = struct
   type 'a stream = 'a S.t
   
   (* take *)
   let take = S.take

   (* head *)
   let head = S.head

   (* filter *)
   let filter = S.filter

   (* map *)
   let map = S.map

   (* zip *)
   let zip = S.zip

   (* from *)
   let rec from n =
      print_endline ("step " ^ string_of_int n) ;
      S.Cons ( n,  S.delay (fun () -> from (n+1) ))

   (* nats *)
   let nats = from 1

   (* cubes_from *)
   let rec cubes_from (n : int) : int stream =
       S.Cons (n*n*n , S.delay(fun () -> cubes_from (n+1)))

   (* zeros stream *)
   let zeros =
       let rec zero_h () =
       S.Cons (0, S.delay zero_h)
       in zero_h ()

   (* 3.2 cubes_from_zip function *)
   let rec cubes_from_zip (n : int) : int stream =
       let zip_stream = zip (+) zeros nats
       in
       match zip_stream with
       | S.Cons (h, tl) -> if (n = h + (n-1))
                        then S.Cons (n*n*n , S.delay (fun () -> cubes_from_zip (n+1)))
                        else raise(Failure "this should not happen")

    (* natural from 0 stream *)
    let nats_0 = from 0
    
    (* 3.3 cube_from_map function *)
    let rec cubes_from_map (n : int ) : int stream =
        let map_stream = map (fun x-> x+1) nats_0
        in
        match map_stream with
        | S.Cons(h, tl) -> if ( n = h + (n-1))
                        then S.Cons (n*n*n , S.delay (fun () -> cubes_from_zip (n+1)))
                        else raise(Failure "this should not happen")

    (* 3.4 drop function *)
    let rec drop (n : int) (s : 'a stream) : 'a stream =
         match n,s with
         | 0, _ -> s
         | n, S.Cons (h,tl) -> drop (n-1) (S.demand tl)

    (* 3.5 drop_until function *)
    let rec drop_until (f : 'a -> bool) (s : 'a stream) : 'a stream =
         match s with
         | S.Cons (h,tl) ->
                       match f h with
                       | true -> S.Cons (h,tl)
                       | false -> drop_until f (S.demand tl)

    (* foldr function *)
    let rec foldr f s =
         match s with
         | S.Cons (h,tl) -> f h (S.delay (fun () -> foldr f (S.demand tl)))

    (* and_fold function *)
    let and_fold (b : bool stream) : bool =
         let and_fun =
                fun x1 x2 ->
                       if x1 then (x1 && (S.demand x2)) else false
                       in
                       foldr and_fun b

    (* sum_positive_prefix function *)
    let rec sum_positive_prefix (i : int stream) : int =
          let plus_fun =
                   fun x1 x2  ->
                           if x1 >= 0 then ((+) x1 (S.demand x2)) else 0
                           in
                           foldr plus_fun i

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
          | S.Cons (h, tl) ->
            S.Cons(h, S.delay(fun () -> sieve (sift h S.(demand tl))))

    let primes = sieve (from 2)

end
