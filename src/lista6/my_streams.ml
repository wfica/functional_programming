type 'a stream = EoS | SC of 'a * 'a stream Lazy.t;;

let peek stream = 
  match stream with 
  | EoS -> None
  | SC(v, _) -> Some(v)
;;

let next stream  = 
  match stream with 
  | EoS -> failwith "end of stream"
  | SC(_, lazy tl) -> tl 
;;

let pop_front stream = 
  match stream with 
  | EoS -> failwith "end of stream"
  | SC(v, lazy tl) -> v, tl
;;


