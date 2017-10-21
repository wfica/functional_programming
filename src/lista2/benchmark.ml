open Core_bench;;

let rec reverse l =
  match l with
  | [] -> []
  | hd::tl -> reverse tl @ [hd]
  
let run_bench () =
  Bench.bench
    [Bench.Test.create ~name:"reverse" (fun () -> ignore (reverse [1;2;3;4;5;6]));
     Bench.Test.create ~name:"List.rev" (fun () -> ignore (List.rev [1;2;3;4;5;6]))]
;;
