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

module type DICTIONARY =
sig 
  type key
  type 'a t 
  exception DuplicatedKey of key
  val empty: unit -> 'a t
  val lookup: 'a t -> key -> 'a option
  val insert: 'a t -> key -> 'a -> 'a t
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

(*!!  jak ładnie dodać jakieś hashowanie, dziedziczenie, bardziej sparametryzować??*)
module Dictionary (Hash: KEY_HASHING):  DICTIONARY with type key = Hash.t =
struct
  type key = Hash.t
  type 'a t = Tip | Node of int * 'a * 'a t * 'a t
  exception DuplicatedKey of key
  let empty () = Tip

  let sign x = if x = 0 then EQ else if x > 0 then GT else LT 

  let lookup tree key = 
    let rec _lookup tree hkey = 
      match tree with
      | Tip -> None
      | Node(hk, value, t1, t2) ->
        match sign @@ compare hkey hk with 
        | LT -> _lookup t1 hkey
        | EQ -> Some value
        | GT -> _lookup t2 hkey
    in _lookup tree (Hash.hash key)


  let insert tree key value = 
    let rec _insert tree hkey value =
      match tree with
      | Tip -> Node(hkey, value, Tip, Tip)
      | Node(hk, v, t1, t2) -> 
        match sign @@ compare hkey hk with 
        | LT -> Node(hk, v, _insert t1 hkey value, t2)
        | EQ -> raise (DuplicatedKey key)
        | GT -> Node(hk, v, t1, _insert t2 hkey value) 
    in _insert tree (Hash.hash key) value
end;;



module HashDict = Dictionary(IntHash);;


(*!!  jak zrobić funkcję, która będzie przyjmować dict:DICTIONARY ??*)
let memoize f =
  let cache = ref  (HashDict.empty () ) in 
  let f_memo = fun x ->
    match HashDict.lookup !cache x with
    | Some y -> y 
    | None ->
      let res = f x in
      cache := HashDict.insert !cache x res ;
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
┌──────────┬──────────────┬─────────┬─────────┐
│ Name     │     Time/Run │ mWd/Run │ mGC/Run │
├──────────┼──────────────┼─────────┼─────────┤
│ fib      │ 512_556.89ns │         │         │
│ fib_memo │     155.03ns │  17.00w │ 0.06e-3 │
└──────────┴──────────────┴─────────┴─────────┘
*)
