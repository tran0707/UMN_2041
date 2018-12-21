(*Name: Khoa Tran -Group Name: Quoc Le, JIngtao Yu*)
(*Readinfile function*)
let read_file (file_name: string) : char list =
    let ic = open_in file_name 
    in 
    let rec read_chars ic =
      try 
         let next_char = input_char ic
         in next_char :: read_chars ic
      with 
          _ -> [] 
    in read_chars ic

(*Concat list of string*)
let implode (cs: char list) : string =
      String.concat "" (List.map  (String.make 1) cs)

(*split*)
let split fs lst =
   let accum =([],[])
   in
   let f(pre,cur) x =
      if fs x 
          then (List.rev cur::pre, [])
          else (pre,x::cur)
       in
       let(ps,cs) = List.fold_left f accum lst
          in 
          List.rev(List.rev cs :: ps)

(*file path *)
let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"

(*Answers function *)
let answers str =   (* string -> string list*)
     let read = read_file str 
     in
     let spliting = split( fun x -> x = '\n'|| x =' ') read
     in
     let cleaning = List.filter(fun x -> List.length x != 0) spliting 
     in
     let get_words = List.map( fun x -> implode x ) cleaning
     in
     let extract6  = List.filter(fun y -> String.length y = 6) get_words 
     in
     let extract4 = List.filter(fun d -> String.length d = 4) get_words
     in
     List.filter(fun i -> List.mem (String.sub i 1 4) extract4) extract6 

(*
let answers str =   (* string -> string list*)
     let file = read_file str 
     in
     let spliting = split( fun x -> x = '\n'|| x =' ') file
     in
     let cleaning = List.filter(fun x -> List.length x != 0) spliting 
     in
     let get_words = List.map( fun x -> implode x ) cleaning
     in
     let extract6  = List.filter(fun y -> String.length y = 6) get_words 
     in
     let extract4 = List.filter(fun d -> String.length d = 4) get_words
     in
     List.filter(fun i -> List.mem (String.sub i 1 4) extract4) extract6 
*)

(*pretty_answers function*)
let pretty_answers lst = (*string list ->(string *string) list *)
    List.map(fun x -> (String.sub x 1 4 ,x)) lst
