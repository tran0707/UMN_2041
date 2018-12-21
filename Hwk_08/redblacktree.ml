(*Some of the below code was build upon the foundation of 
 * Professor Eric Van Wyk's lecture note*)
open Ordered

module type RedBlackSetSig = sig
    type elem
    type color = R | B
    type t = E | T of color * t * elem * t

    val empty : t
    val insert : elem -> t -> t
    val member : elem -> t -> bool
    val isRedBlackTree: t -> bool
end

module RedBlackTree (O : OrderedSig) :
            (RedBlackSetSig with type elem = O.t) = struct
    type elem = O.t
    type color = R | B
    type t = E | T of color * t * elem * t
    let eq = O.eq
    let lt = O.lt
    let leq = O.leq
    let empty = E
    let rec member elem tree =
        match elem, tree with
        | x, E -> false
        | x, T(_, a, y, b) ->
                        if x < y
                        then member x a
                        else if x > y
                        then member x b
                        else true
    let balance color t1 v t2 =
        match color, t1, v, t2 with
        | B, T(R, T(R,a,x,b), y,c), z, d
        | B, T(R, a,x, T(R,b,y,c)), z, d
        | B, a, x, T(R, T(R,b,y,c), z, d)
        | B, a, x, T(R, b,y, T(R,c,z,d))  -> T(R, T(B,a,x,b), y, T(B,c,z,d))
        | a ,b ,c ,d -> T (a ,b ,c ,d)
    let insert x s =
        let rec ins t = match t with
        | E -> T(R, E, x, E)
        | T(color, a, y, b) ->
                            if x < y
                            then balance color (ins a) y b
                            else if x > y
                            then balance color a y (ins b)
                            else t
        in
        match ins s with
        | E -> raise (Failure "FAIL")
        | T(_, a, y, b) -> T(B, a, y, b)


(* //////////// isRedBlackTree helper functions /////////////*)

(* Red-black trees has some invariants:
    * + In red-black tree, if a parent node is a red node, then
    *   its child can't be a red child.
    * + All paths from the root will have the same number of black nodes.
    * + It is a binary search tree. Meaning that each child has max 2
    *   children and left node elements are smaller their parent elemnt.
    *   the right node elements are greater than their parent element.
 * *)
     exception FAIL_INVARIANTS
     (*check will return BAD_TREE if red nodes has red children
      *or different # of black nodes else check will return # black nodes*)
     (* /////////EXTRA CREDIT EXPLAINATION://///////////
      * A tree has left tree and right tree.
      * FOR LEFT TREE: check_trees function will +1 to the number of
      * black nodes if inside of left tree contains a tree with a black
      * node without explicitly generating all paths. This progress
      * keep +1 to black nodes if inside of the left tree still contain
      * a different black node. FOR RIGHT TREE: The same process from
      * above will count number of black node inside of right tree.
      * After done with counting the total black nodes on both left
      * and right tree, it will compare the number black nodes inside
      * of left vs right tree to see if they have the same number of black
      * nodes. If the result returns false (they are different),
      * then raise exception. If it returns true, then the final result
      * will be the total number of black tree of the left tree + 1 if tree
      * (not left or right) is a black node else not + 1.
      *)
     let rec check_rbtrees (t_lst : t) : int =
         match t_lst with
         | E -> 0
         | T(R,T(R,_,_,_),_,_) -> raise FAIL_INVARIANTS  (*check two adjacent
                                                   nodes have same red color*)
         | T(R,_,_,T(R,_,_,_)) -> raise FAIL_INVARIANTS  (*check two adjacent
                                                   nodes have same red color*)
         | T(co, left, el, right) -> let black_node = check_rbtrees left in
                                        if (check_rbtrees right) <> black_node
                                        then raise FAIL_INVARIANTS
                                        else
                                            if (co = B)
                                            then black_node + 1 (*+1,co = B*)
                                            else black_node

     let rec map_count_child_tree f t =
     (* map on both left and right node *)
         match t with
         | E -> true
         | T(co,c1,e,c2) -> (f c1) && (f c2) && (map_count_child_tree f c1)
                                             && (map_count_child_tree f c2)

     let rec map_left_tree f t =
     (* map on child left node *)
         match t with
         | E -> true
         | T(co,c1,e,c2) -> (f c1) && (map_left_tree f c1)

     let rec map_right_tree f t =
     (* map on child right node *)
         match t with
         | E -> true
         | T(co,c1,e,c2) -> (f c2) && (map_right_tree f c2)

     let check_num_child_helper (trees : t) : bool =
     (* bool function to find number of children under parent *)
         match trees with
         | E -> true
         | T(_,c1,_,c2) -> if (c1 = E || c2 = E) then true
                         else if (c1 = E && c2 <> E) then true
                         else if (c1 <> E && c2 = E) then true
                         else if (c1 <> E && c2 <> E) then true
                         else false

     let get_elem (trees : t) : elem =
     (* get element of given tree *)
         match trees with
         | E -> raise(Failure "NO ELEM TO GET")
         | T(_,_,e,_) -> e

     let check_left_helper (trees : t) : bool =
     (* check if left element is < parent element *)
         match trees with
         | E -> true
         | T(_,c1,e,c2)-> if (c1 = E || c2 = E) then true
                         else if (c1 = E && c2 <> E) then true
                         else if (c1 <> E && c2 = E) then true
                         else if (c1 <> E && c2 <> E) then get_elem c1 < e
                         else false

     let check_right_helper (trees : t) : bool =
     (* check if right element is < parent element *)
         match trees with
         | E -> true
         | T(_,c1,e,c2)-> if (c1 = E || c2 = E) then true
                         else if (c1 = E && c2 <> E) then true
                         else if (c1 <> E && c2 = E) then true
                         else if (c1 <> E && c2 <> E) then get_elem c2 > e
                         else false


     let check_num_child trees = map_count_child_tree check_num_child_helper trees
     (* check if total children is < 3*)
     let check_left trees = map_left_tree check_left_helper trees
     (* check if left elemnt is < parent element *)
     let check_right trees = map_right_tree check_right_helper trees
     (* check if right elemnt is > parent element *)
     let isBinarySearchTree trees =
     (* if all 3 checks above pass -> it's binary search tree*)
                             (check_num_child trees)
                             (* total number of child of each parents < 3 *)
                          && (check_left trees)
                             (* left elemnet is < parent element *)
                          && (check_right trees)
                             (* right elemnt is > parent element *)

     let isRedBlackTree (rbtrees: t) : bool =
         (
         try ignore (check_rbtrees rbtrees); true with   (*ignore if it return
                                                      the # black nodes,*)
         | FAIL_INVARIANTS -> false     (*if check return FAIL_INVARIANTS
                                          then return FAIL_INVARIANTS*)
         )
       && isBinarySearchTree rbtrees
end

module RBTI = RedBlackTree (Int)
(*TEST CASES*)
let h1  = RBTI.empty
let h2  = RBTI.insert 20 h1
let h3  = RBTI.insert 30 h2
let h4  = RBTI.insert 15 h3
let h5  = RBTI.insert 40 h4
let h6  = RBTI.insert 50 h5
let h7  = RBTI.insert 10 h6
let h8  = RBTI.insert  8 h7
let h9  = RBTI.insert  7 h8
let h10 = RBTI.insert  9 h9
let h11 = RBTI.insert 18 h10
let h12 = RBTI.insert 14 h11
let h13 = RBTI.insert 51 h12
let h14 = RBTI.insert 52 h13
let h15 = RBTI.insert 53 h14
let h16 = RBTI.insert 35 h15
let h17 = RBTI.insert 33 h16

let m1 = RBTI.member 20 h5              (*TRUE*)
let m2 = RBTI.member 40 h5              (*TRUE*)
let m3 = RBTI.member 50 h5              (*FALSE*)

let check1 = RBTI.isRedBlackTree h3     (*TRUE*)
let check2 = RBTI.isRedBlackTree h4     (*TRUE*)
let check3 = RBTI.isRedBlackTree h5     (*TRUE*)
let check4 = RBTI.isRedBlackTree h3     (*TRUE*)
let check5 = RBTI.isRedBlackTree h17    (*TRUE*)
let check6 = RBTI.isRedBlackTree (RBTI.T (RBTI.R , RBTI.E, 20,
                            RBTI.T (RBTI.R, RBTI.E, 30, RBTI.E))) (*FALSE*)
