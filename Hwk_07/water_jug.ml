(* The Water Jug Problem - 2 jug (4 gallon and 3 gallon),
 * fill 2 gallons into the 4 gallon jug *)

(* I represent the state by type int * int to count the amount of water
 * inside of each jugs. The first int will be for jug4 and the other is
 * for jug3. Poss_move function will create a (state * operation) list
 * to search for the next possible move. Also, it updates the value of
 * state based on the operation that it takes. play function uses 
 * List.iter to choose the next possible move and check on the state 
 * using final function.
 *)

type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug

type state = int * int
(* state has int * int to easy keep track of the amount of water
 * inside of each jugs.*)

let final (s : state) : bool =
    match s with
    |(j4,j3) -> (j4 == 2) && (j3 == 0) 


(* describe function *)
let describe (four:int) (three:int) : string =
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

let ok_state ((j3,j4),opr : state * operation) : bool =
    if j4 < 0 || j3 < 0 then false else true

let poss_move (sta : state) : (state * operation) list =
    (* poss_move; generating a next (state*operation) list based on the current state *)
    let fill4GallonJugFromTap (j4,j3) =  if (j4 < 4) && (j4 <> 4) 
                                         then [((4,j3),Fill4GallonJugFromTap)] else []
    in
    let fill3GallonJugFromTap (j4,j3) = if (j3 < 3) && (j3 <> 3) 
                                        then [((j4,3),Fill3GallonJugFromTap)] else []
    in
    let empty4GallonJugOnGround (j4,j3) = if (j4 > 0) 
                                          then [((0,j3),Empty4GallonJugOnGround)] else []
    in
    let empty3GallonJugOnGround (j4,j3) = if (j3 > 0) 
                                          then [((j4,0),Empty3GallonJugOnGround)] else []
    in
    let fill4GallonJugFrom3GallonJug (j4,j3) = if ((j3 > 0) && (j4 + j3 > 4))
                              then [((4,j3 -(4-j4)),Fill4GallonJugFrom3GallonJug)] else []
    in
    let fill3GallonJugFrom4GallonJug (j4,j3)= if ((j4 > 0) && (j3 + j4 > 3))
                                then [((j4-(3-j3),3),Fill3GallonJugFrom4GallonJug)] else []
    in
    let empty4GallonJugInto3GallonJug (j4,j3) = if ((j4 > 0) && (j3 + j4 <= 3))
                                    then [((0,j3+j4),Empty4GallonJugInto3GallonJug)] else []
    in
    let empty3GallonJugInto4GallonJug (j4,j3)= if ((j3 > 0) && (j3 + j4 <= 4))
                                    then [((j3+j4,0),Empty3GallonJugInto4GallonJug)] else []
    in
    List.filter ok_state (fill4GallonJugFromTap sta @ fill3GallonJugFromTap sta @
                          empty4GallonJugOnGround sta @ empty3GallonJugOnGround sta @
                          fill4GallonJugFrom3GallonJug sta @ fill3GallonJugFrom4GallonJug sta @
                          empty4GallonJugInto3GallonJug sta @ empty3GallonJugInto4GallonJug sta)

let rec print_out (sta_opr_lst : (state *operation) list) : (operation * string) list =
    (* print_out: print out the list of operation and the current state of the two jugs *)
        match sta_opr_lst with
        | [] -> []
        | ((s1,s2),op) :: tl  -> [(op, describe s1 s2)] @ print_out tl

exception FoundMove of (state * operation) list

let is_not_element set v = not(List.mem v set)

let play () : (operation * string) list option = 
    let rec let_play state path =
        if final state 
        then raise (FoundMove path)
        else
            let valid_move = List.filter (is_not_element path) (poss_move state)
            in
            let go_fun (sta, op) = let_play sta (path @ [(sta,op)])
            in
            List.iter go_fun valid_move
    in
    try let_play (0,0) [] ; None
    with FoundMove path -> Some (print_out path)








