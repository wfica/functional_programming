open Util;;

let insert_everywhere x xs =
  let rec _insert x xs acc cnt =
    match cnt with
    | -1 -> acc
    | _ -> 
      let one_insertion = take cnt xs @ [x] @ drop cnt xs in
      _insert x xs (one_insertion :: acc ) (cnt-1)  in
  _insert x xs [] (length xs)
;;

let rec put_everywhere   x xss =
  match xss with
  | [] -> [  ]
  | hd::tl -> (insert_everywhere x hd) @ (put_everywhere x tl)
;;

let rec perms list = 
  match list with
  | [] -> [[]]
  | hd :: tl -> put_everywhere hd @@ perms tl
;;


let test () =
  try
    assert (perms [] = [[]]) ;
    assert ( perms [1] = [[1]]);
    assert ( perms [1;2;3] = [[1; 2; 3]; [2; 1; 3]; [2;3;1];[1;3;2];[3;1;2];[3;2;1]] ) ;
    print_string "success\n"
  with 
  | _ -> print_string "failure\n"
;;