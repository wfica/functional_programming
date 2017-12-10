open Core.Std

module type PQUEUE =
sig
  type elem
  type queue

  exception EmptyPQueue

  val is_empty : queue -> bool
  val empty : queue
  val insert : queue  -> elem -> queue 
  val remove : queue  -> elem * queue
  val peek : queue -> ( elem * queue ) option
end     


module type ORDTYPE =
sig
  type t
  val compare : t -> t -> int
end  

let create_comparable (type a) cmp =
  (module struct
    type t = a
    let compare = cmp
  end : ORDTYPE with type t = a)


module PQueue (Order:ORDTYPE) : PQUEUE with type elem := Order.t =
struct
  type elem = Order.t
  type queue = elem list
  exception EmptyPQueue 
  let is_empty q = q = [] 
  let empty = []
  let rec insert q e =
    if is_empty q  then [e] 
    else if Order.compare e (List.hd_exn q) >= 0  then e ::q 
    else (List.hd_exn q) :: (insert (List.tl_exn q) e)
  let remove q =
    match q with 
    | [] -> raise EmptyPQueue
    | hd::tl -> (hd, tl) 
  let peek q = if is_empty q then None else Some (remove q)
end 

module type SORT = 
sig
  type elem 
  val sort : elem list -> elem list
end

module QueueSort (Order:ORDTYPE) :SORT with type elem := Order.t = 
  struct
    type elem = Order.t
    module PQ = PQueue(Order) 
    open PQ
    let list_to_queue l  =
      List.fold l ~init:empty ~f:(fun acc x -> insert acc x) 
    let queue_to_list q = 
      let rec take_out q acc =
        match peek q with
        | None -> acc
        | Some(hd,tl) -> take_out tl (hd::acc)
      in take_out q []
    let sort l = 
      list_to_queue l |> queue_to_list 
  end



let sort (type a) (module O : ORDTYPE with type t = a) (l : a list) =
  let module QS = QueueSort(O) in 
  QS.sort l 



let test_case (lazy (a, b)) number =
  try 
    assert (a = b)
  with 
  | e ->  Core.Std.printf "test number %i failed" number ;
    raise e 


let test () =
  let int_ord =  create_comparable Int.compare in
  let string_ord = create_comparable String.compare in
  try 
    test_case  (lazy([], sort int_ord [])) 0 ;
    test_case  (lazy([2],sort int_ord [2])) 1 ;
    test_case  (lazy([1;2;4], sort int_ord [4;2;1]  )  ) 2 ;
    test_case  (lazy( [-10;-9;3;100], sort int_ord [100;-10;3;-9])) 3 ;

    test_case  (lazy([], sort string_ord [])) 4 ;
    test_case  (lazy(["a"],sort string_ord ["a"])) 5 ;
    test_case  (lazy(["Ala"; "I"; "Kota"; "Ma"], sort string_ord ["Ala"; "Ma"; "Kota"; "I"]  )  ) 6 ;

    true
  with
  |_ -> false

