open Core.Std;;
let hd stream = stream 0;;
let tl stream = fun (i : int ) -> stream (i+1);;
let map stream f = fun (i : int ) -> f ( stream i );;
let add stream c  = map stream ( (+) c) ;;
let map2 s1 s2 f = fun (i : int )  -> f  (s1 i) (s2 i);;
let replace stream n a =  fun (i : int ) -> 
    if i mod n = 0 then a
    else stream i;;
let take stream n = fun  (i : int) -> stream (n*i);;
let fold stream f a = fun (i : int) ->  
  let rec my_fold stream f a n =
    let first = f a (hd stream) in
     if n = 0 then first else my_fold (tl stream) f first (n-1) in
  my_fold stream f a i;;

let rec tabulate stream ?(b= 0) e = 
    if b = e then []
    else (hd  stream ) :: (tabulate (tl stream ) ~b:(b+1) e  )
;;
(* core banch, kolejność argumentów,  *)

let s1 = fun (x : int)  ->  float x ;;
print_float  @@ hd s1 ;;
let s2 = tl  s1;;
print_float @@  hd s2;;
let s3 = fun (i : int) -> i;;
hd s3 |> print_int;;
fold s3 (+) 0 10 |> print_int;;
let s4 = add s3 (-1);;
List.iter  ~f:( fun x-> print_endline @@ string_of_int x )  ( tabulate s4 ~b:0 10 );;
print_endline;;
let s5 = map2 s3 s4 ( * );;
List.iter  ~f:( fun x-> print_endline @@ string_of_int x )  ( tabulate s5 ~b:0 10 );;
print_endline;;









