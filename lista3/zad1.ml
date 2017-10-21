(* 
zad 1
Załóżmy, że wielomiany o współczynnikach rzeczywistych są reprezentowane 
jako listy współczynników od najwyższej potęgi do najniższej, np. 
[1.;0.;-1.;2.] oznacza wielomian x3 - x + 2. Napisz funkcję, która dla 
zadanej listy reprezentującej wielomian i dla danego argumentu x typu float, 
obliczy wartość tego wielomianu w punkcie x w oparciu o schemat Hornera. 
Napisz tę funkcję w dwóch wersjach: raz za pomocą rekursji ogonowej, a 
następnie bez jawnego użycia rekursji, korzystając z odpowiedniej funkcji 
bibliotecznej modułu List.
*)
open Core.Std;;
open Core_bench;;

let eval_polynomial l (x : float) =
  let rec _eval l x acc =
    match l with 
    | [] -> acc
    | hd :: tl -> _eval tl x (x *. acc +. hd)
  in _eval l x 0. 
;;

let eval_polynomial2 l (x : float) = 
  List.fold l ~f:(fun acc a -> x *. acc +. a ) ~init: 0.  

let test () =
  try 
    assert ( eval_polynomial [] 12. = 0. ) ;
    assert ( eval_polynomial [0.] 13. = 0. ) ;
    assert ( eval_polynomial [3.] (-1.) = 3. );
    assert ( eval_polynomial [1.; 0.; 0.] (-3.) = 9.) ;
    assert ( eval_polynomial [-1.; 0.; 2.; 0.5] 2. = -3.5) ;

    assert ( eval_polynomial2 [] 12. = 0. ) ;
    assert ( eval_polynomial2 [0.] 13. = 0. ) ;
    assert ( eval_polynomial2 [3.] (-1.) = 3. );
    assert ( eval_polynomial2 [1.; 0.; 0.] (-3.) = 9.) ;
    assert ( eval_polynomial2 [-1.; 0.; 2.; 0.5] 2. = -3.5) ;
    true;
  with
  | _ -> false;
;; 

let run_bench () =
  let poly = 
    let rec _gen_poly n acc = 
      match n with
      | 0 -> acc
      | _ -> _gen_poly (n-1) (Random.float 2. :: acc)
    in _gen_poly 10000 [] 
  in
  Bench.bench
    [Bench.Test.create ~name:"tail recursion" (fun () -> eval_polynomial poly (1.)) ;
     Bench.Test.create ~name:"List.fold " (fun () -> eval_polynomial2 poly (1.)) ]
;;
(*
┌────────────────┬──────────┬─────────┬──────────┬──────────┬───────────┐
│ Name           │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │   mGC/Run │
├────────────────┼──────────┼─────────┼──────────┼──────────┼───────────┤
│ tail recursion │ 267.28us │ 40.00kw │    0.62w │    0.62w │ 152.62e-3 │
│ List.fold      │ 424.25us │ 40.00kw │    0.78w │    0.78w │ 152.60e-3 │
└────────────────┴──────────┴─────────┴──────────┴──────────┴───────────┘
*)