(*Name : Khoa Tran -Group Name: Quoc Le and     Jingtao Yu*)
let p1 ="Hello world!\n\n How are you today? \t\t I hope all is well. "
let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

let implode (cs: char list) : string =
      String.concat "" (List.map  (String.make 1) cs)

let read_file (file_name: string) : char list =
      let ic = open_in file_name 
      in 
      let rec read_chars ic =
      try 
      let next_char = input_char ic
       in next_char :: read_chars ic
        with 
         _-> [] 
        in read_chars ic

let explode (s: string) : char list =
      let l = String.length s
      in
      let rec f i = 
         if i = l then [] else s.[i] :: f (i+1)
      in f 0

let split fs lst = (*'a->bool)->a'list->'a list list*)
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

(*format function*)
let format str  width = (*string -> int -> string*)
    let char_list = explode str
    in 
    let spliting =  split(fun x-> x = '\n' || x= '\r'||  x = '\t' || x = ' ' ) char_list
    in
    let cleaned_list = List.filter(fun x-> List.length x != 0) spliting
    in 
    let string_list = List.map(fun x-> implode x) cleaned_list
    in
    let tuple_list = List.map(fun x-> (x,String.length x)) string_list
    in
    let accum = ("",0)
    in
    let f (cur,size) (str,len) =
        if size = 0 
            then (str,len)
        else if (size + 1 + len) <= width
            then (cur ^ " " ^ str,size + 1 + len)
        else
            (cur ^ "\n" ^ str,len)
    in
    let (result_string, final_len) = List.fold_left f accum tuple_list
    in
    result_string
(*
let format str  width = (*string -> int -> string*)
    let char_list = explode str
    in 
    let spliting =  split(fun x-> x = '\n' ||  x = '\t' || x = ' ' ) char_list
    in
    let cleaned_list = List.filter(fun x-> List.length x != 0) spliting
    in 
    let string_list = List.map(fun x-> implode x) cleaned_list
    in
    let tuple_list = List.map(fun x-> (x,String.length x)) string_list
    in
    let accum = ("",0)
    in
    let f (cur,size) (str,len) =
        if size = 0 
            then (str,len)
        else if (size + 1 + len) <= width
            then (cur ^ " " ^ str,size + 1 + len)
        else
            (cur ^ "\n" ^ str,len)
    in
    let (result_string, final_len) = List.fold_left f accum tuple_list
    in
    result_string
    *)
