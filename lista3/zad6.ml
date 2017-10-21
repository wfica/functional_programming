(*
zad 6
Pewnego razu powiedziano dwóm logikom P i S, że wybrano dwie liczby naturalne 
x i y takie, że 1 < x < y oraz x+y < 100. Co więcej, S otrzymał informację 
o wartości x+y, a P – o wartości x*y. Po chwili P i S odbyli następującą 
rozmowę: 

P: Nie potrafię powiedzieć jakie to liczby. 
S: Wiedziałem o tym. 
P: A to w takim razie już potrafię. 
S: Ja już też. 

Napisz program, którego treścią będzie powyższy dialog, a wartością – 
para liczb x i y.
*)
open Core.Std;;

let rec zip_with_elem ~l elem =
  match l with
  | [] -> []
  | hd::tl -> (elem, hd):: zip_with_elem ~l:tl elem
;;

let rec gen_pairs l1 l2 =
  List.map l1 ~f: (zip_with_elem ~l:l2) |>
  List.fold ~init:[] ~f:List.append
;;

let ( **| ) f g x = f ( g x) ;;

let memoize f =
  let table = Hashtbl.Poly.create () in
  (fun x ->
     match Hashtbl.find table x with
     | Some y -> y
     | None ->
       let y = f x in
       Hashtbl.add_exn table ~key:x ~data:y;
       y
  )
;;
let filter1 (a, b) = a + b < 100 && a < b;;

(*returns [(a,b) | 1 < a < b < x and x = 0 (mod a) and x = 0 mod b] *)
let proper_factors = 
  let _proper_factors x= 
    let sq = x |> float |> sqrt |> int_of_float in
    let num = if sq * sq = x then List.range 2 sq else List.range 2 (sq+1) in
    let left_divisors = List.filter num ~f:( fun y -> x mod y = 0 ) in
    List.map left_divisors ~f:(fun y -> (y, x / y) ) |>
    List.filter ~f: filter1 
  in memoize _proper_factors 
;;


(*returns [(a,b) | 1 < a < b < x and a + b = x *)
let proper_splits =
  let _proper_splits x = 
    let half = ( x + x mod 2) / 2 in
    List.range 2 half |>
    List.map ~f:(fun y -> (y, x-y) ) |>
    List.filter ~f: filter1 
  in memoize _proper_splits
;;

let filter2 x = List.length @@ proper_factors  x > 1;;
let filter3 x = 
  proper_splits x |>
  List.map ~f:(fun (a, b) -> a * b) |>
  List.for_all ~f:filter2
;;

let filter4 x = 
  proper_factors x |>
  List.map ~f:(fun (a,b) -> a + b ) |>
  List.count ~f:filter3 = 1
;;

let filter5 x =
  proper_splits x |>
  List.map ~f:(fun (a,b) -> a * b) |>
  List.count ~f:filter4 = 1
;;

let solve () =
  let x = List.range 2 99 
  and y = List.range 2 99 in
  gen_pairs x y  |>
  List.filter ~f: filter1 |>
  List.filter ~f:( fun (a, b) -> filter2 @@ a*b )|>
  List.filter ~f:( fun (a, b) -> filter3 @@ a+b) |>
  List.filter ~f:( fun (a, b) -> filter4 @@ a*b) |>
  List.filter ~f:( fun (a, b) -> filter5 @@ a+b) 
;;
