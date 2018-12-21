(* Explaning my solution
 * 1. The search space of my solution is based on the check_valid_color function, 
 *    List.length test, and the remain node of the in the input argument. First, I try
 *    color C 1 for the first node, if this color is not certify one of the condition
 *    above, then I will try color C 2 for that same node, then C 3. This process will
 *    repeat until all the nodes in list is complete. At the end, if it has a return 
 *    result, then it will print out the option else none will print out.
 * 2. The adj_same_color_check function will take care of the two adjacent nodes are 
 *    colored with the same color. I call this function right after the color_option
 *    and color_exception function declare. This way, they will stop even go into the 
 *    searching process if the two node in edges list have the same color.
*)

type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

let g1 = ( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )
let g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]

(* HELPER FUNCTIONS FOR color_option and color_exception *)
(* ///////////////////////////////////////////////////// *)
(* get_color: get color of giving node *)
let rec get_color  (n : node) (c : coloring) : color option =
    match c with
    | [] -> None
    | (nd,c):: cs -> if (n = nd)
                     then Some c
                     else get_color n cs

(* adjacency_list: find all node in egde list that are negibor with that node *)
let rec adjacency_list (n: node) (g:graph) : edge list = 
    match n,g with
    | n ,(x,(e1,e2)::es) -> if (n = e1) || (n = e2)
                           then [(e1,e2)] @ adjacency_list n (x,es)
                           else
                               adjacency_list n (x,es)
    |_,_ -> []

(* adj_color_determin: use the output of adjacency_list function and get_color
 * function to check the color of two neighbor node are the same color or not *)
let rec adj_color_determin (e : edge list) (c : coloring) : bool = 
        match e with
        | (n1,n2):: ns -> (match get_color n1 c, get_color n2 c with
                           | None, None -> true
                           | Some color1, None -> true
                           | None, Some color2 -> true
                           | Some color1 ,Some color2 -> color1 <> color2
                          )
                          && adj_color_determin ns c
        | [] -> true

(* check_valid_color: will check if a color that used with a node is valid.
 * A coloring is valid if all egde have nodes that is not color at all or
 * color it different *)
let rec check_valid_color (c : coloring) (g : graph) : bool =
    match c with
    | (nd,co)::cs ->
                    let adj_lst = adjacency_list nd g  
                    in
                    (adj_color_determin adj_lst c) && check_valid_color cs g
    | [] -> true

(* adj_same_color_check: check if two adjacent nodes are colored with the same color *)
let rec adj_same_color_check (g : graph) : bool = 
    match g with
    | nd_lst, (e1,e2) :: es -> (e1 = e2) || adj_same_color_check (nd_lst, es)
    | _,[] -> false

(* /////////////////////////////////////////////////////// *)

(* color_option function *)
let color_option ((nd , ed): graph) : coloring option =
    if (adj_same_color_check (nd,ed))
    then None
    else
    let rec try_color coloring_lst rest =
       if (check_valid_color coloring_lst (nd,ed)) 
          && ((List.length nd) = (List.length coloring_lst)) 
          && rest = []
       then
           Some (List.rev (coloring_lst))
       else
           match rest with
           | [] -> None
           | n :: ns -> match try_color ((n , C 1) :: coloring_lst) ns with
                        | Some result -> Some result
                        | None -> match try_color ((n , C 2) :: coloring_lst) ns with
                                  | Some result -> Some result
                                  | None -> match try_color ((n , C 3) :: coloring_lst) ns with
                                             | Some result -> Some result
                                             | None -> None
    in
    try_color [] nd 

(* color_exception function *)
exception FoundColoring of  coloring

let color_exception ((nd,ed) : graph) : unit = 
    if (adj_same_color_check (nd,ed))
    then ()
    else
    let rec try_color coloring_lst rest =
       if (check_valid_color coloring_lst (nd,ed)) 
          && ((List.length nd) = (List.length coloring_lst)) 
          && rest = []
       then
           raise (FoundColoring (List.rev (coloring_lst)))
       else
           match rest with
           | [] -> ()
           | n :: ns -> try_color ((n , C 1) :: coloring_lst) ns;
                        try_color ((n , C 2) :: coloring_lst) ns;
                        try_color ((n , C 3) :: coloring_lst) ns
    in
    try_color [] nd 
