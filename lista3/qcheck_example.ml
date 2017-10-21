open QCheck;;

let rec rev_append l acc =
  match l with
  | [] -> acc
  | hd::tl -> rev_append tl (hd::acc)

let rev l = rev_append l []

let rec rev_append_wrong l acc =
  match l with
  | [] -> List.rev acc
  | hd::tl -> rev_append_wrong tl (hd::acc)

let test1 = QCheck.Test.make ~name:"rev_append" 
                             (QCheck.pair QCheck.(list small_int) QCheck.(list small_int))
                             (fun (l, acc) -> List.rev l @ acc = rev_append l acc)

let test2 = QCheck.Test.make ~name:"rev_rev_id" 
                             QCheck.(list small_int)
                             (fun l -> rev (rev l) = l)

let test3 = QCheck.Test.make ~name:"rev_append_wrong" 
                             (QCheck.pair QCheck.(list small_int) QCheck.(list small_int))
                             (fun (l, acc) -> List.rev l @ acc = rev_append_wrong l acc)


let run_tests () = QCheck_runner.run_tests_main [test1; test2; test3]
