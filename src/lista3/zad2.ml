(* 
zad 2
Załóżmy, że zmieniamy reprezentację wielomianu tak, by współczynniki w 
liście były uszeregowane od najniższej potęgi do najwyższej (wtedy 
[1.;0.;-1.;2.] oznacza wielomian 2x3 - x2 + 1). Zmodyfikuj obie funkcje z 
Zadania 1 tak, by poprawnie liczyły wartość wielomianu w tej reprezentacji 
(rekursja w rozwiązaniu z jawną rekursją nie musi być ogonowa).
*)
open Core.Std;;
open Core_bench;;

let eval_rev_polynomial l (x : float) =
  let rec _eval l x =
    match l with 
    | [] -> 0.
    | hd :: tl -> hd +. x *. _eval tl x 
  in _eval l x 
;;

let eval_rev_polynomial2 l (x : float) = 
  List.fold_right l ~f:(fun a acc -> x *. acc +. a ) ~init: 0.  
;;

let test () =
  try 
    assert ( eval_rev_polynomial [] 12. = 0. ) ;
    assert ( eval_rev_polynomial [0.] 13. = 0. ) ;
    assert ( eval_rev_polynomial [3.] (-1.) = 3. );
    assert ( eval_rev_polynomial [0.; 0.; 1.] (-3.) = 9.) ;
    assert ( eval_rev_polynomial [0.5; 2.; 0.; -1.] 2. = -3.5) ;

    assert ( eval_rev_polynomial2 [] 12. = 0. ) ;
    assert ( eval_rev_polynomial2 [0.] 13. = 0. ) ;
    assert ( eval_rev_polynomial2 [3.] (-1.) = 3. );
    assert ( eval_rev_polynomial2 [0.; 0.; 1.] (-3.) = 9.) ;
    assert ( eval_rev_polynomial2 [0.5; 2.; 0.; -1.] 2. = -3.5) ;
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
    in _gen_poly 10000 [] in
  Bench.bench
    [Bench.Test.create ~name:"not tail-recursion" (fun () -> eval_rev_polynomial poly (1.)) ;
     Bench.Test.create ~name:"List.fold_right " (fun () -> eval_rev_polynomial2 poly (1.)) ]
;;
(*
┌────────────────┬──────────┬─────────┬──────────┬──────────┬───────────┐
│ Name           │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │   mGC/Run │
├────────────────┼──────────┼─────────┼──────────┼──────────┼───────────┤
│ tail recursion │ 267.28us │ 40.00kw │    0.62w │    0.62w │ 152.62e-3 │
│ List.fold      │ 424.25us │ 40.00kw │    0.78w │    0.78w │ 152.60e-3 │
└────────────────┴──────────┴─────────┴──────────┴──────────┴───────────┘
┌────────────────────┬──────────┬─────────┬───────────┬───────────┬───────────┬──────────┐
│ Name               │ Time/Run │ mWd/Run │  mjWd/Run │  Prom/Run │   mGC/Run │ mjGC/Run │
├────────────────────┼──────────┼─────────┼───────────┼───────────┼───────────┼──────────┤
│ not tail-recursion │ 299.84us │ 40.00kw │     0.62w │     0.62w │ 152.62e-3 │          │
│ List.fold_right    │ 760.62us │ 70.01kw │ 4_006.11w │ 4_006.11w │ 267.08e-3 │  0.59e-3 │
└────────────────────┴──────────┴─────────┴───────────┴───────────┴───────────┴──────────┘
*)