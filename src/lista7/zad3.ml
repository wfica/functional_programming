(*
Zadanie 3 (6p.)

Technika memoizacji pozwala wykorzystać cechy imperatywne języka w celu zwiększenia 
efektywności działania funkcji, która sama jest czysto funkcyjna, tj. kolejne wywołanie takiej 
funkcji dla tego samego argumentu zwróci tę samą wartość. Memoizacja polega na zapamiętywaniu 
wartości wywołań funkcji dla konkretnych argumentów w pewnej strukturze danych, i na 
wyszukiwaniu potrzebnych wartości przy kolejnych wywołaniach tej funkcji. Aby umożliwić 
memoizację dowolnej jednoargumentowej funkcji (o typie argumentu, którego wartości OCaml potrafi 
porównywać), zaimplementuj następujący schemat: 

zdefiniuj typ polimorficzny służący jako tablica wartości wywołań dowolnej funkcji;
napisz funkcję tworzenia pustej tablicy;
napisz funkcję wyszukiwania w tablicy wartości funkcji dla zadanego argumentu;
napisz funkcję dopisującą do tablicy nową wartość wywołania funkcji.

Wykorzystaj ten schemat do memoizacji funkcji wyznaczającej kolejne liczby Fibonacciego: napisz 
funkcję fib : int -> int według standardowej definicji oraz funkcję fib_memo : int -> int 
wykorzystującą memoizację. Porównaj czasy działania obu funkcji. 

Czy funkcja fib_memo spełnia Twoje oczekiwania w kwestii efektywności? Zaimplementuj taką 
wersję funkcji fib_memo, która  lepiej wykorzysta technikę memoizacji.
*)

type ordering = LT | EQ | GT ;;

module type ORDER =
sig
  type t
  val compare: t -> t -> ordering
end ;;

module type KEY_HASHING =    
sig 
  type t
  val hash: t -> int
end ;;

module IntHash : KEY_HASHING with type t = int = 
struct
  type t = int
  let hash key = 
    let fk  = float key in 
    (fk *. 0.618034 -. floor (fk *. 0.618034) ) *. 32768. |> int_of_float
end;;

module type DICTIONARY =
sig 
  type key
  type 'a t 
  exception DuplicatedKey of key
  val empty: unit -> 'a t
  val lookup: 'a t -> key -> 'a option
  val insert: 'a t -> key -> 'a -> 'a t
end ;;

module Dictionary (Key: ORDER):  DICTIONARY with type key = Key.t  =
struct
  type key = Key.t
  type 'a t = Tip | Node of key * 'a * 'a t * 'a t
  exception DuplicatedKey of key
  let empty () = Tip
  let rec lookup tree key = 
    match tree with
    | Tip -> None
    | Node(k, value, t1, t2) ->
      match Key.compare key k with 
      | LT -> lookup t1 key
      | EQ -> Some value
      | GT -> lookup t2 key


  let rec insert tree key value =
    match tree with
    | Tip -> Node(key, value, Tip, Tip)
    | Node(k, v, t1, t2) -> 
      match Key.compare key k with 
      | LT -> Node(k, v, insert t1 key value, t2)
      | EQ -> raise (DuplicatedKey key)
      | GT -> Node(k, v, t1, insert t2 key value) 
end;;


module IntOrder : ORDER with type t = int = 
struct
  type t = int
  let compare a b = 
    let cmp = Pervasives.compare a b in 
    if cmp < 0 then  LT else if cmp > 0 then GT else EQ
end;;

module IntDict = Dictionary(IntOrder);;



let memoize f =
  let cache = ref  (IntDict.empty () ) in 
  let f_memo = fun x ->
    match IntDict.lookup !cache x with
    | Some y -> y 
    | None ->
      let res = f x in
      cache := IntDict.insert !cache x res ;
      res 
in f_memo
;;

let rec fib n = if n > 0 then fib(n-2) + fib (n-1) else 1;;

let fib_memo =  memoize fib ;;


let test_case (lazy (a, b)) number =
  try 
    assert (a = b)
  with 
  | e ->  Core.Std.printf "test number %i failed" number ;
    raise e 
;;

let test () =

  try 
    Core.Std.List.iter ~f:(fun i -> test_case( lazy(fib_memo i, fib i) ) i)  [0;1;2;3;4;5;6] ;
    true
  with
  |_ -> false
;;

open Core_bench;;

let run_bench () =
  let fib_memo_bench = memoize fib in 
  Bench.bench
    [Bench.Test.create ~name:"fib" (fun () -> ignore (fib 20));
     Bench.Test.create ~name:"fib_memo" (fun () -> ignore ( fib_memo_bench 20 ))]
;;
(*
┌──────────┬──────────────┬─────────┐
│ Name     │     Time/Run │ mWd/Run │
├──────────┼──────────────┼─────────┤
│ fib      │ 520_436.42ns │         │
│ fib_memo │      88.40ns │   2.00w │
└──────────┴──────────────┴─────────┘
*)
