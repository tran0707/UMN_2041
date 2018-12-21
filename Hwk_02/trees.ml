(*///////////////////// PART A ////////////////////*)

(*   TREE_TYPE   *)
type 'a tree = Leaf of 'a |Fork of 'a * 'a tree * 'a tree

(*    TEST CASE   *)
let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))
let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

(*   t_size function   *)
let rec t_size t =
    match t with 
    |Leaf a -> 1
    |Fork(f,h,t)-> 1 + t_size h + t_size t

(*  t_sum function   *)
let rec t_sum t =
    match t with
    |Leaf a -> a
    |Fork(f,h,t)-> f + t_sum h + t_sum t

(*   t_charcount function   *)
let rec t_charcount t =
    match t with 
    |Leaf a -> String.length(a)
    |Fork(f,h,t)-> String.length(f) + t_charcount h + t_charcount t 

(*   t_concat function   *)
let rec t_concat (t:string tree):string=
    match t with
    |Leaf a -> a
    |Fork(f,h,t)-> f ^ t_concat h ^ t_concat t

(*//////////////////// PART B ////////////////////*)

(*  OPTION t_size function   *)
let rec t_opt_size t =
    match t with 
    |Leaf (Some a) -> 1
    |Fork(Some f, h, t)-> 1 + t_opt_size h + t_opt_size t
    |_-> 0

(*  OPTION t_sum function   *)
let rec t_opt_sum t =
    match t with
    |Leaf (Some a) -> a
    |Fork(Some f, h, t)-> f + t_opt_sum h + t_opt_sum t
    |_-> 0
(*   OPTION t_charcount function   *)
let rec t_opt_charcount t =
    match t with 
    |Leaf (Some a) -> String.length(a)
    |Fork(Some f,h, t)-> String.length(f) + t_opt_charcount h + t_opt_charcount t 
    |_->0
(*  OPTION t_concat function   *)
let rec t_opt_concat t=
    match t with
    |Leaf (Some a) -> a
    |Fork(Some f,  h,t)-> f ^ t_opt_concat h ^ t_opt_concat t
    |_->""

(*//////////////////// PART C ///////////////////*)

(*   tfold function   *)    
let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b = 
    match t with
    | Leaf v -> l v
    | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

(*  tf_size function   *)
let tf_size t = tfold (fun a -> 1)(fun a b c-> 1 + b + c) t  

(*   tf_sum function   *)
let tf_sum t = tfold (fun a -> a)(fun a b c -> a + b + c) t

(*   tf_charcount function    *)
let tf_charcount t = tfold (fun a-> String.length(a))(fun a b c -> String.length(a) + b + c) t

(*   tf_concat function    *)
let tf_concat t = tfold (fun a-> a)(fun a b c->  a ^ b ^c) t

(*   tf_opt_size function   *)
let tf_opt_size t =
    let leaf l =
        match l with
        |Some a -> 1
        |_ -> 0
    in
    let fork a b c = 1 + b + c
    in
    match t with
    |Leaf Some _ -> 1
    |Fork(_,_,_) -> tfold leaf fork t
    |_ -> 0


(*   tf_opt_sum function   *)
let tf_opt_sum t =
    let leaf l =
        match l with 
        |Some a -> a
        |_ -> 0
    in
    let fork a b c =
       match a with
       |Some a -> a + b +c
       |_ -> b + c
    in 
    match t with 
    |Leaf Some _ -> 1
    |Fork (_,_,_)-> tfold leaf fork t
    |_ -> 0 

(*   tf_opt_charcount function   *)
let tf_opt_charcount t = 
    let leaf l =
        match l with 
        |Some a ->String.length(a)
        |_ -> 0
    in
    let fork a b c =
       match a with
       |Some a -> String.length(a) + b +c
       |_ -> b + c
    in 
    match t with 
    |Leaf Some a -> String.length (a) 
    |Fork (_,_,_)-> tfold leaf fork t
    |_ -> 0 

(*   tf_opt_concat function   *)
let tf_opt_concat t =
    let leaf l =
        match l with 
        |Some a -> a
        |_ -> ""
    in
    let fork a b c =
       match a with
       |Some a -> a ^ b ^ c
       |_ -> b ^ c
    in 
    match t with 
    |Leaf Some a -> a 
    |Fork (_,_,_)-> tfold leaf fork t
    |_ -> "" 

(*/////////////////////// PART D //////////////////////*)

(*   TREE_TYPE   *)
type 'a btree = Empty | Node of 'a btree * 'a * 'a btree

(*   TEST CASE   *)
let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

(*   bt_insert_by function   *)
let rec bt_insert_by compare element tree =
    match tree with
    |Empty -> Node(Empty, element, Empty)
    |Node(left,value,right)-> if compare element value < 0
                               then Node(bt_insert_by compare element left,value,right)
                               else if compare element value >0
                               then Node(left,value,bt_insert_by compare element right)
                               else Node(left,value,right)

(*   bt_element_by function   *)
let rec bt_elem_by func elem tree =
    match tree with
    |Empty -> false
    |Node(left,value,right)  -> if func elem value = true then true
                                else bt_elem_by func elem left 
                                ||   bt_elem_by func elem right

(*   bt_to_list function   *)
let rec bt_to_list tree =
    match tree with
    |Empty -> []
    |Node(left,value,right)  -> bt_to_list left  @ [value] @ bt_to_list right

(*   btfold function   *)
let rec btfold (x:'b) (func:'b->'a->'b->'b) (tree:'a btree):'b =
    match tree with
    |Empty-> x
    |Node(tl,node,tr)-> func (btfold x func tl) (node) (btfold x func tr)

(*   btf_elem_by function   *)
let btf_elem_by func elem tree =
    let f acc1 node acc2 = func node elem || acc1 || acc2
    in
    match tree with
    |Empty -> false
    |Node(tl,value,tr) -> btfold false f tree 

(*   btf_to_list function   *)
let btf_to_list tree =
    let f acc1 node acc2 = acc1 @ [node] @ acc2
    in
    match tree with
    |Empty -> []
    |Node(tl,value,tr) -> btfold [] f tree

(*   btf_insert_by comment   *)
(*
 * val btfold : 'b -> ('b -> 'a -> 'b -> 'b) -> 'a btree -> 'b = <fun>
 * val btf_insert_by : ('a -> 'a -> int) -> 'a -> 'a btree -> 'a btree = <fun>
 * Try to write it:
     * let btf_insert_by func elem tree =
         * match tree with
         * |Empty -> (Empty,elem,Empty)
         * |Node(left,value,right) -> bt_fold(X)(fun acc1 value acc2-> Y) tree 
* bt_fold will be difficult to use in the btf_insert_by because the X location is require the base case,which will be the node that we want to insert. The result after the apply the base case to bt_fold will be the next base case for the next call, which is what we don't want to happen. This will cause the btf_insert_by difficult to implement it.  
*)




























