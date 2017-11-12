open Core.Std

exception Non_pal;;


let is_pal l =
  let rec _is_pal slow fast =
    match fast, slow with
    | [], _  -> (true , slow)
    | [_], _ -> (true , List.tl_exn slow)
    | _::_::f_tl, hd::tl -> 
      let ans, elem = _is_pal    tl f_tl in
      if ans = false || hd <> List.hd_exn elem 
      then raise Non_pal  
      else (true, List.tl_exn elem)
    | _ -> failwith "unreachable"
  in  try 
    _is_pal l l |> fst
  with 
  | _ -> false
;;

