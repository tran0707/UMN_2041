(*Some of the below code was build upon the foundation of 
 *Professor Eric Van Wyk's lecture note*)
open Ordered

module type BinomialHeapSig = sig
    type elem
    type tree = Node of int * elem * tree list
    type t = tree list
    val empty : t
    val isEmpty : t -> bool
    val insert : elem -> t -> t
    val merge : t -> t -> t
    val findMin : t -> elem
    val findMinDirect: t -> elem
    val deleteMin : t -> t
    val isBinomialTree : tree -> bool
    val isBinomialHeap : t -> bool
end

module BinomialHeap (E : OrderedSig) :
                 (BinomialHeapSig with type elem = E.t) = struct
     type elem = E.t
     type tree = Node of int * elem * tree list
     type t = tree list
     let eq = E.eq
     let lt = E.lt
     let leq = E.leq

     let empty = []

     let isEmpty ts =
         match ts with
         | [] -> true
         | _ -> false

     let rank (Node(r,x,c)) = r

     let root (Node(r,x,c)) = x

     let link t1 t2 =
         match t1,t2 with
         | Node(r,x1,c1), Node(_,x2,c2) ->
                 if (E.leq x1 x2)
                 then Node(r+1, x1, t2::c1)
                 else Node(r+1, x2, t1::c2)

     let rec insTree t1 t2 =
         match t1,t2 with
         |t, [] -> [t]
         |t,ts -> match ts with
                  | t' :: ts' ->
                    if rank t < rank t'
                    then t::ts
                    else insTree (link t t') ts'
                  | [] -> [t]

     let insert x ts = insTree (Node (0,x,[])) ts

     let rec merge ts1 ts2 =
         match ts1,ts2 with
         |ts1,[] -> ts1
         |[],ts2 -> ts2
         | ts1, ts2 -> match ts1,ts2 with
                       | t1::ts1', t2::ts2'->
                            if rank t1 < rank t2
                            then t1 :: merge ts1' ts2
                            else if rank t2 < rank t1
                                then t2 :: merge ts1 ts2'
                                    else insTree (link t1 t2)
                                    (merge ts1' ts2')
                       |_,ts2' -> ts2'

     let rec removeMinTree trees = match trees with
          | [t] -> (t,[])
          | t :: ts ->
                    let (t1, t1s) = removeMinTree ts
                    in if root t < root t1
                         then (t, ts)
                         else (t1, t::t1s)
          | [] -> raise (Failure "Nothing left to remove")

     let findMin ts = let (t,_) = removeMinTree ts in root t

     let deleteMin ts =
            let (Node (_,_,ts1), ts2) = removeMinTree ts
            in merge (List.rev ts1) ts2

    (*////// isBinomialTree/isBinomialHeap healper functions //////*)
     let count_elem (t : tree) : int =
      (*count the total number of node in tree*)
         let rec check hd_node =
             let Node (_,_,tree_lst) = 
                 hd_node in
                 let rec helper t_lst =
                    match t_lst with
                    | [] -> 0
                    | hd :: tl -> (check hd) + (helper tl) in
                  1 + helper tree_lst 
          in check t

    let find_child (t : tree) : int =
      (*find child of tree*)
        match t with
        | Node(r,x,ts) -> 
                        let rec helper t_lst = 
                            match t_lst with
                            | [] -> 0
                            | hd :: tl -> 1 + helper tl
                         in helper ts 

    let root_lst (t_lst : t) : elem list=
      (*generate a list of root by applying root function to all tree list*)
        let rec helper tr_lst = 
            match tr_lst with
            | [] -> []
            | hd :: tl -> root(hd) :: helper tl
        in
        helper t_lst
     
    let smallest_root (lst : elem list) : elem =
      (* find smallest root from a input of a list of root*)
        let a_helper min x = 
           match min with
           | None -> Some x
           | Some a -> if a < x then Some a else Some x
        in
        match (List.fold_left a_helper None (lst)) with
        | Some i -> i
        | _ -> raise(Failure "Can't find min")

    let findMinDirect (t_lst : t)  : elem  = 
      (*findMinDirect without calling findMin or deleteMin*)     
                smallest_root (root_lst t_lst) 


    let rec less_than_lst (lst : int list) : bool =
      (*check the rank in one tree is in a decreasing order *)
      (* the input will be a rank list*)
        match lst with
        | [] -> true
        | [h] -> true
        | h1 :: h2 :: t -> if( h1 > h2)
                           then less_than_lst ( h2 :: t)
                           else false

     let rec check_elem (trees : tree) : bool = 
       (*check children nodes is >= to parent node*)
         match trees with
         | Node(_,e,lst) -> let rec helper1 t1 =
                            match t1 with
                           | [] -> true
                           | hd :: tl -> if(E.leq e (root hd) && helper1 tl)
                                         then (check_elem hd && helper1 tl)
                                         else 
                                             false
                            in helper1 lst

     let rec map func (lst : tree list) = 
       (* map over the children node*)
         match lst with
         | [] -> []
         | Node(_,_,l) :: tl -> (func l) :: (map func tl)

(* The invariants of a binomial tree are:
    * + If tree has rank 0, then that tree will not have any children.
    *   It leads to the number of element in that tree will be 1 only.
    * + Tree with rank r + 1 will be the result of linking two tree
    *   with rank r.
    * + The total number of elements in a tree will be equal to
    *   2 ^(rank of that tree)
    * + A binomial tree of rank r is a node with r children, and
    *   rank of children will be in decreasing order.
    * + The children node have to be greater than or equal to their
    *   parent nodes.
*)
    let rec isBinomialTree (t : tree) : bool =
        match t with
        | Node(r,x,cs) -> if r = 0 then r = (count_elem t -1) else 
                       (*rank 0 will has zero children*)
                        (((r+1) = rank (link (Node(r,x,cs)) (Node (r,x,cs))))
                       (*r+1 tree contail 2 tree have same rank*) 
                        && (float_of_int (count_elem t)) =
                                        (2.0 ** (float_of_int r))
                       (*# elements = 2^rank*)
                        && ( r = find_child t)
                       (*rank = number of children*)
                        && (let rec check_child (t_ll : t) : bool =
                             match t_ll with
                             | [] -> true
                             | lst -> 
                              (less_than_lst(r :: (List.map rank cs)))
                            && List.fold_left (&&) true (map check_child lst)
                        in
                        check_child cs)
                       (* rank in a tree is in decreasing order*)
                        && ( check_elem t))
                       (* children is greater than or equal to their parent *)

    (*///// isBinomialHeap helper functions /////*)
    let rec greater_equal_lst (el_l : int list) : bool = 
      (*check the rank of different tree in tree list is 
       * in an increasing order(greater than or equal)*)
        match el_l with
        | [] -> true
        | [h] -> true
        | h1 :: h2 :: t -> if (h1 <= h2) 
                           then greater_equal_lst (h2::t)
                           else false
(*
 * The invariants of a binomial heap (a set of binomial trees) are:
    * + every trees inside of tree heap is a binomial tree.
    * + The tree list is stored in increasing order of rank.
 *)

    let rec isBinomialHeap tree_lst =
       (let rec helper t_l = 
            match t_l with
            | [] -> true
            | hd :: tl -> (isBinomialTree hd) && (helper tl) 
        in
        helper tree_lst)
     && (greater_equal_lst(List.map rank tree_lst))
end

module BHI = BinomialHeap(Int)
(*TEST CASES*)
let h1 =  BHI.empty
let h2 =  BHI.insert 20 h1
let h3 =  BHI.insert 30 h2
let h4 =  BHI.insert 10 h3
let h5 =  BHI.insert 40 h4
let h7 =  BHI.insert 80 h5
let h8 =  BHI.insert 100 h7
let h9 =  BHI.insert 90 h8
let h10 = BHI.insert 95 h9

let h6 = BHI.deleteMin h5     

let m1 = BHI.findMin h9           (*10*)
let m2 = BHI.findMin h6           (*20*) 
let m3 = BHI.findMinDirect h6     (*20*)

let m3 = BHI.isBinomialHeap h4    (*TRUE*)
let m4 = BHI.isBinomialHeap h5    (*TRUE*)
let m5 = BHI.isBinomialHeap h6    (*TRUE*)
let m6 = BHI.isBinomialHeap ([BHI.Node (100, 80, []);   (*FALSE*)
                             BHI.Node (2, 10,[BHI.Node (1, 20,
                             [BHI.Node (0, 30, [])]);
                             BHI.Node (0, 40, [])])])

let m7 = BHI.isBinomialTree (BHI.Node (0,10,[]))                    (*TRUE*)
let m8 = BHI.isBinomialTree (BHI.Node (2, 10, [BHI.Node (1, 20, 
                [BHI.Node (0, 30, [])]); BHI.Node (0, 40, [])]))    (*TRUE*)
let m9 = BHI.isBinomialTree (BHI.Node (2, 10, [BHI.Node (1, 200,   (*FALSE*)
                            [BHI.Node (0, 30, [])]); BHI.Node (0, 40, [])])) 
let m10 = BHI.isBinomialTree (BHI.Node (3, 10, [BHI.Node (2, 80,     (*TRUE*)
                            [BHI.Node (1, 90, [BHI.Node (0, 95, [])]);
                            BHI.Node (0, 100, [])]); BHI.Node (1, 20,
                            [BHI.Node (0, 30, [])]); BHI.Node (0, 40, [])]))

