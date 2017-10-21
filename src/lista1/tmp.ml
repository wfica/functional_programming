3;;

3.4;;

'c';;

"ccc";;

false;;

();;

let f7 (x,y,z) =
  if x then
  y 
  else 
  z

;;

let f (x,(y,z))=
  ((x,y),z) ;;

let rec flatten list=
  match list with
  | [] -> []
  | x :: xs -> x @ (flatten xs)  
;;

let rec revert_acc list acc =
  match list with
  | [] -> acc
  | x :: xs -> revert_acc xs (x :: acc )


let revert list =
  revert_acc list []


let palindrom list =
  let rev_list = revert list in
  rev_list = list 
;;

let l1 = [1;2;3] in
let l2 = [1;2;3] in
l1 = l2
;;
open Core.Std;;

let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;
let ff () =
  String.split ~on:':' path
  |> List.dedup ~compare:String.compare
  |> List.iter ~f:print_endline
;;
let rec get (x::xs) n =
  if n == 0 then x else get xs (n-1);;

let ten = 
  let rec enum n = n :: (enum (n + 1) ) in
  get (enum 10) ;;