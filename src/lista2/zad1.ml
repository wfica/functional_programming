open Util;;

let rec sub_lists list  =
  match list with
  | [] -> [[]]
  | hd :: tl -> 
    let tmp = sub_lists tl in
    let rec push_front list x acc =
      match list with 
      | [] -> acc
      | hd :: tl -> push_front tl x ((x :: hd) :: acc)
    in push_front tmp hd tmp
;;


sub_lists [1;2;3;4];;

let test () =
  try
    assert ( sub_lists [] = [[]]) ; 
    assert (sub_lists [1] = [[1];[]]) ;
    assert ( sub_lists[1;2] = [ [1]; [1; 2]; [2]; [] ]  ) ; 
    print_string "success\n" 
  with
  | _ -> print_string "failure\n"
;;
