open Core.Std

module type PQUEUE =
sig
  type priority
  type 'a t

  exception EmptyPQueue

  val is_empty : 'a t -> bool
  val empty : 'a t
  val insert : 'a t -> priority -> 'a -> 'a t
  val remove : 'a t -> priority * 'a * 'a t
  val peek : 'a t -> (priority * 'a * 'a t ) option
end     


module type ORDTYPE =
sig
  type t
  type comparison = LT | EQ | GT

  val compare : t -> t -> comparison
end  

module PQueue (Order:ORDTYPE) : PQUEUE with type priority = Order.t =
struct
  type priority = Order.t
  type 'a t = ('a * priority )list
  exception EmptyPQueue 
  let is_empty q = q = [] 
  let empty = []
  let insert queue  p v  =  (v,p)::queue
  let remove queue = 
    let get_max idx acc current = 
      match acc with 
      | None -> Some(idx, current)
      | Some(_, v) ->
        if snd current > snd v then Some(idx, current)
        else acc 
    in
    match List.foldi queue ~f:get_max ~init:None with
    | None -> raise EmptyPQueue 
    | Some(idx, (value, prior)) -> (prior, value, List.take queue idx @ List.drop queue (idx+1))
  let peek q = if is_empty q then None else Some (remove q)
end 

module IntOrder : ORDTYPE with type t = int =
struct 
  type t = int
  type comparison = LT | EQ | GT
  let compare x y  = 
    let res = compare x y in 
    if res > 0 then GT else if res = 0 then EQ else LT 
end

module IntQueue = PQueue(IntOrder)

let int_list_to_queue l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd::tl -> aux tl (IntQueue.insert acc hd hd)
  in aux l IntQueue.empty


let int_queue_to_list q = 
  let rec aux q acc =
    match IntQueue.peek q with
    | None -> acc
    | Some (_, value, rem_q) -> aux rem_q (value::acc)
  in aux q []


let int_sort l =
  int_list_to_queue l |>
  int_queue_to_list


let test_case (lazy (a, b)) number =
  try 
    assert (a = b)
  with 
  | e ->  Core.Std.printf "test number %i failed" number ;
    raise e 


let test () =
  try 
    test_case  (lazy([], int_sort [])) 0 ;
    test_case  (lazy([2], int_sort [2])) 1 ;
    test_case  (lazy(int_sort[4;2;1],  [1;2;4])) 2 ;
    test_case  (lazy(int_sort[-9;3;-10;100],  [-10;-9;3;100])) 3 ;

    true
  with
  |_ -> false

