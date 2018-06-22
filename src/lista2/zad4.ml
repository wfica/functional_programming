open Util;;
open Core_bench;;

let rec merge cmp l1 l2 =
  match l1 with 
  | [] -> l2
  | hd1 :: tl1 -> match l2 with 
    | [] -> l1
    | hd2::tl2 -> 
      if cmp hd1 hd2 then hd1 :: merge cmp tl1 l2
      else hd2 :: merge cmp l1 tl2
;;

let mergeT cmp l1 l2 =
  let rec _merge cmp l1 l2 acc =
    match l1, l2 with
    | [], []  -> rev acc
    | [], hd2 :: tl2 -> _merge cmp [] tl2 (hd2::acc)
    | hd1 :: tl1, [] ->  _merge cmp tl1 [] (hd1::acc)
    | hd1::tl1, hd2 :: tl2 -> 
      if cmp hd1 hd2 then _merge cmp tl1 l2 (hd1::acc)
      else _merge cmp l1 tl2 (hd2::acc)
  in _merge cmp l1 l2 []    
;;

let merge_sort cmp list =
  let rec _merge_sort cmp list n =
    match n with
    | 0 | 1 -> list
    | _ -> 
      let a = _merge_sort cmp (take  (n /2) list  ) (n/2) 
      and b = _merge_sort cmp (drop (n/2) list ) (n - n/2) 
      in mergeT cmp a b
  in _merge_sort cmp list (length list)
;;

let merge_no_rev cmp l1 l2 =
  let rec _merge cmp l1 l2 acc =
    match l1, l2 with
    | [], []  ->  acc
    | [], hd2 :: tl2 -> _merge cmp [] tl2 (hd2::acc)
    | hd1 :: tl1, [] ->  _merge cmp tl1 [] (hd1::acc)
    | hd1::tl1, hd2 :: tl2 -> 
      if cmp hd1 hd2 then _merge cmp tl1 l2 (hd1::acc)
      else _merge cmp l1 tl2 (hd2::acc)
  in _merge cmp l1 l2 []    
;;


let merge_sort_no_rev cmp list =
  let rec _merge_sort cmp list n =
    match n with
    | 0 | 1-> list
    | _ -> 
      let cmp2 x y =  not @@ cmp x y in
      let a = _merge_sort cmp2 (take  (n /2) list  ) (n/2) 
      and b = _merge_sort cmp2 (drop (n/2) list ) (n - n/2) 
      in merge_no_rev cmp a b
  in _merge_sort (fun x y -> not @@ cmp x y) list (length list)
;;


let test () =
  try 
    let array = [1; 0; 5; 2; 9; 0; 1; 10]
    and corr = [0; 0; 1; 1; 2; 5; 9; 10] in 
    assert (merge_sort (<=) array = corr) ;
    assert (merge_sort (>=) array = rev corr) ;
    assert ( merge_sort (<=) [] = []) ;
    print_string "success testing merge_sort\n" ;
    assert (merge_sort_no_rev (<=) [] = []) ; 
    assert (merge_sort_no_rev (<=) [9] = [9]) ;
    assert (merge_sort_no_rev (<=) [1;3] = [1;3]) ;
    assert (merge_sort_no_rev (<=) [2;1] = [1;2]) ;
    assert (merge_sort_no_rev (<=) [2;3;1] = [1;2;3]) ;
    assert (merge_sort_no_rev (<=) [9;4;2;6;3] = [2;3;4;6;9]) ;
    print_string "success testing merge_sort_rev\n" ;
  with 
  | _ -> print_string "failure\n"
;;



let run_bench () =
  let list = rev_range 1000 in
  Bench.bench
    [Bench.Test.create ~name:"revert" (fun () -> ignore (merge_sort (<=) list));
     Bench.Test.create ~name:"no revert" (fun () -> ignore (merge_sort_no_rev (<=) list))]
;;
(*
Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
┌───────────┬────────────┬─────────┬───────────┬───────────┬───────────┬──────────┐
│ Name      │   Time/Run │ mWd/Run │  mjWd/Run │  Prom/Run │   mGC/Run │ mjGC/Run │
├───────────┼────────────┼─────────┼───────────┼───────────┼───────────┼──────────┤
│ revert       │ 1_151.62us │ 99.45kw │ 1_058.88w │ 1_058.88w │ 379.36e-3 │  0.15e-3 │
│ no revert │   510.56us │ 39.59kw │   221.49w │   221.49w │ 151.04e-3 │          │
└───────────┴────────────┴─────────┴───────────┴───────────┴───────────┴──────────┘
*)






