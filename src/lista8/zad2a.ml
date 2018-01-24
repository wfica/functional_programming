(* 
Zadanie 2 (8p.)

Rozważmy sygnaturę dla funkcyjnych kolejek priorytetowych:

  module type PQUEUE =
  sig
    type priority
    type 'a t

    exception EmptyPQueue

    val empty : 'a t
    val insert : 'a t -> priority -> 'a -> 'a t
    val remove : 'a t -> priority * 'a * 'a t
  end     

1. Zdefiniuj moduł PQueue : PQUEUE, przyjmując typ priority = int. Reprezentacja kolejki 
może być dowolna.
2. Wykorzystaj moduł PQueue do napisania funkcji sortowania list liczb typu int.
3. Uogólnij rozwiązanie punktów 1 i 2 definiując funktor,
który dla zadanego modułu OrdType : ORDTYPE zwraca moduł o sygnaturze PQUEUE, gdzie

  module type ORDTYPE =
  sig
    type t
    type comparison = LT | EQ | GT

    val compare : t -> t -> comparison
  end     

Zmodyfikuj odpowiednio funkcję sortowania list z p. 2 i przetestuj ją.
Przy pomocy modułów pierwszego rodzaju zdefiniuj funkcję sort, która dla dowolnego 
modułu implementującego sygnaturę ORDTYPE dla pewnego typu i dla dowolnej listy elementów 
tego typu posortuje listę zgodnie z porządkiem definiowanym przez moduł. Użyj funktora z 
poprzedniego punktu. Poza modułami pierwszego rodzaju będzie potrzebne jeszcze jedno 
rozszerzenie języka. *)


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
;;

module PqueueInt:PQUEUE with type priority = int = 
struct
  type priority = int
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
;;


let int_list_to_queue l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd::tl -> aux tl (PqueueInt.insert acc hd hd)
  in aux l PqueueInt.empty
;;

let int_queue_to_list q = 
  let rec aux q acc =
    match PqueueInt.peek q with
    | None -> acc
    | Some (_, value, rem_q) -> aux rem_q (value::acc)
  in aux q []
;;

let int_sort l =
  int_list_to_queue l |>
  int_queue_to_list
;;

let test_case (lazy (a, b)) number =
  try 
    assert (a = b)
  with 
  | e ->  Core.Std.printf "test number %i failed" number ;
    raise e 
;;

let test () =
  try 
     test_case  (lazy([], int_sort [])) 0 ;
     test_case  (lazy([2], int_sort [2])) 1 ;
     test_case  (lazy(int_sort[4;2;1],  [1;2;4])) 2 ;
     test_case  (lazy(int_sort[-9;3;-10;100],  [-10;-9;3;100])) 3 ;

    true
  with
  |_ -> false
;;
