(* #use "common/util.ml";; *)
open Util;;

let cycle list n =
  let rec _cycle list n acc =
    match n with 
    | 0 -> list @rev acc
    | _ ->  match list with
               | hd :: tl -> _cycle tl (n-1) (hd :: acc)
               | [] -> [] (* unreachable *)
    in _cycle list (length list - n) []
;;

let test () =
  try 
    let array = [0;1;2;3;4;5] in 
    assert (cycle array 0 = array) ;
    assert (cycle array 6 = array) ;
    assert (cycle array 5 = [1;2;3;4;5;0] ) ;
    assert (cycle array 4 = [2;3;4;5;0;1] ) ;
    print_string "success\n"
  with 
  | _ -> print_string "failure\n"
;;
