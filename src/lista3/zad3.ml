(* 
zad 3
Dla reprezentacji wielomianu z Zadania 2 napisz funkcję (ponownie w dwóch 
wersjach) obliczającą pochodną wielomianu (np. dla listy [1.;0.;-1.;2.] 
funkcja ta powinna utworzyć listę [0.;-2.;6.]).
*)
open Core.Std;;
open Core_bench;;

let derivative_polynomial l =
  let rec _eval l n = 
    match l with 
    | [] -> []
    | hd :: tl -> n *. hd :: _eval tl (n +. 1.)
    in _eval (List.drop l 1)  1.
;;

let derivative_polynomial2 l = 
  List.mapi (List.drop l 1) ~f: (fun n a -> (float n +. 1.) *. a) 
;;

let test () =
  try 
    assert ( derivative_polynomial []  = [] ) ;
    assert ( derivative_polynomial [0.] = [] ) ;
    assert ( derivative_polynomial [3.] = [] );
    assert ( derivative_polynomial [0.; 0.; 1.]  = [0.; 2.]) ;
    assert ( derivative_polynomial [0.5; 2.; 0.; -1.]  = [2.; 0.; -3.]) ;
    assert ( derivative_polynomial [1.; 0.; -1.; 2.]   = [0.; -2.; 6.]) ;

    assert ( derivative_polynomial2 []  = [] ) ;
    assert ( derivative_polynomial2 [0.] = [] ) ;
    assert ( derivative_polynomial2 [3.] = [] );
    assert ( derivative_polynomial2 [0.; 0.; 1.]  = [0.; 2.]) ;
    assert ( derivative_polynomial2 [0.5; 2.; 0.; -1.]  = [2.; 0.; -3.]) ;
    assert ( derivative_polynomial2 [1.; 0.; -1.; 2.]   = [0.; -2.; 6.]) ;
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
    [Bench.Test.create ~name:"non-tail derivative" (fun () -> derivative_polynomial poly)  ;
     Bench.Test.create ~name:"List.mapi " (fun () -> derivative_polynomial2 poly ) ]
;;
(*
┌─────────────────────┬────────────┬──────────┬──────────┬──────────┬───────────┬──────────┐
│ Name                │   Time/Run │  mWd/Run │ mjWd/Run │ Prom/Run │   mGC/Run │ mjGC/Run │
├─────────────────────┼────────────┼──────────┼──────────┼──────────┼───────────┼──────────┤
│ non-tail derivative │   489.34us │  70.01kw │   7.44kw │   7.44kw │ 267.06e-3 │  1.17e-3 │
│ List.mapi           │ 1_013.56us │ 120.01kw │  14.30kw │  14.30kw │ 457.75e-3 │  2.31e-3 │
└─────────────────────┴────────────┴──────────┴──────────┴──────────┴───────────┴──────────┘
*)