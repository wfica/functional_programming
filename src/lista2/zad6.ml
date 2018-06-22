open Util;;

let suffixes list =
  let rec _suffixes list acc =
    match list with
    | [] -> acc
    | hd :: tl -> _suffixes tl (list :: acc)
  in rev  @@ _suffixes list []
;;

let prefixes list =rev @@  map rev @@ suffixes @@ rev list;;

let prefixes2 list =
  let rec _prefixes list n acc=
    match n with  
    | 0-> acc
    | _ -> _prefixes list  (n-1) @@ (take n list ) :: acc 
  in _prefixes list (length list) []
;;

let test () =
  try 
    assert ( prefixes [] = []) ;
    assert ( suffixes [] = []) ; 
    assert ( suffixes [1;2;3] = [[1;2;3];[2;3];[3]] ) ; 
    assert ( prefixes [1;2;3] = [[1];[1;2];[1;2;3]]) ; 
    assert ( prefixes2 [1;2;3] = [[1];[1;2];[1;2;3]]) ; 
    assert( prefixes2 [] = []) ;
    print_string "success\n"
  with 
  | _ -> print_string "failure\n"
;;