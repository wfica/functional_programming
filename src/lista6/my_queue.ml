type 'a queue = QC of 'a list * 'a list;;


let push elem ( QC(i, o) )  = QC( elem::i, o);;
let rec pop (QC(i, o)) = 
  match i, o with 
  | [], [] -> failwith "empty queue"
  |  _, hd::tl -> QC(i, tl), hd
  |  _, [] -> pop (QC([], List.rev i))
;;

let is_empty (QC(i, o) )= 
  match i, o with
  | [], [] -> true
  | _, _ -> false
;;

