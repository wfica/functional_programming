open Core_bench;;
open Util;;

let rec a n =
  match n with
  | 0 -> 0
  | _ -> 2 * (a @@ n-1) + 1
;;

let  aa n =  
  let rec _aa n acc =
    match n  with
    | 0 -> acc 
    | _ -> _aa (n-1) (2 * acc + 1)
  in _aa n 0
;;

let test () =
  let args = [0;1;2;3;4;5] in 
  let corr = [0;1;3;7;15;31] in
  let ans1 = map2 (=) (map a args) corr in
  let ans2 = map2 (=) (map aa args) corr in
  not @@ any ans1 false || any ans2 false
;;

let run_bench () =
  Bench.bench
    [Bench.Test.create ~name:"normal" (fun () -> ignore (a 10000));
     Bench.Test.create ~name:"tail recursion" (fun () -> ignore (aa 10000))]
;;




  






