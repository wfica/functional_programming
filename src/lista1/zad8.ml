let comp f g =  fun x -> f (g x)
;;
let rec iter f n = 
  match n with
  | 0 -> fun x -> x
  | _ -> comp f ( iter f (n-1) )
;;
let ( *| ) x y = iter ( ( + )  x )  y 0 
;;
5 *| 4 *| 0 *| 6 *| 3;;
5 *| 4;;

let ( **| ) x y = iter ( ( *| ) x  ) y 1;;

2 **| 3 ;;
2 **| 0;;
0 **| 6 **| 3;;
0 **| 0;;
5 **| 2 **| 3;;
(5 **| 2) **| 3;;
5 **| (2 **| 3);;

