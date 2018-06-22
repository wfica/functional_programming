let rec any list x =
  match list with
  | [] -> false
  | hd :: tl -> if hd = x then true else any tl x
;;

let rev list = 
  let rec rev_acc list acc =
    match list with 
    | [] -> acc
    | hd :: tl -> rev_acc tl @@ hd :: acc
  in rev_acc list []
;;
let length list =
  let rec len list acc =
    match list with
    | [] -> acc
    | _ ::tl -> len tl (acc+1)
  in len list 0
;;
let rec map f list =
  match list with
  | [] -> []
  | hd :: tl -> f hd :: map f tl
;;

let rec map2 f l1 l2 =
  match l1 with 
  | [] -> []
  | hd1 :: tl1 ->
    match l2 with 
    | [] -> []
    | hd2 :: tl2 -> f hd1 hd2 :: map2 f tl1 tl2
;;

let rec drop n list=
  match n, list with
  | 0, _ ->  list
  | _,  hd::tl -> drop  (n-1) tl
;;

let take n list =
  let rec _take list n acc =
    match n, list with 
    | 0, _ -> rev acc 
    | _, hd::tl -> _take tl (n-1) (hd::acc)
  in _take list n []   
;;

let rec rev_range n =
  match n with
  | 0 -> []
  | _ -> n :: ( rev_range (n-1) )