(*
Zadanie 4 (3p.)

Napisz funkcję fresh typu string -> string generującą świeże nazwy, której kolejne wywołania mają następujący efekt: 

# fresh "x";;
- : string = "x1"
# fresh "x";;
- : string = "x2"
# fresh "x";;
- : string = "x3"
# fresh "y";;
- : string = "y4"
itd... oraz funkcję reset typu int -> unit, która ustawia początkową wartość generowanego indeksu dla następnych wywołań funkcji fresh, np. 

# fresh "x";;
- : string = "x1"
# fresh "x";;
- : string = "x2"
# reset 5;;
- : unit = ()
# fresh "x";;
- : string = "x6"
# fresh "x";;
- : string = "x7"

Uwaga! Funkcje nie mogą wykorzystywać żadnych zmiennych globalnych.
*)
;;

let fresh = 
  let idx = ref 0 in
  fun ?(reset=None) x  -> (
      match reset with 
      | Some n -> idx := n  ; ""
      | None  -> 
        idx := !idx + 1 ; 
        x ^ string_of_int !idx
    )
;;

let my_fresh = fresh ~reset:(None) ;;

let reset n = let _ = fresh "" ~reset:(Some n) in  () 
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
    test_case( lazy(fresh "a", "a1") ) 0 ;
    test_case( lazy(fresh "b", "b2") ) 1 ;
    test_case( lazy(fresh "c", "c3") ) 2 ;
    test_case( lazy(fresh "a", "a4") ) 3 ;
    reset 10 ;
    test_case( lazy(fresh "a", "a11") ) 4 ;
    test_case( lazy(fresh "b", "b12") ) 5 ;
    test_case( lazy(fresh "c", "c13") ) 6 ;
    test_case( lazy(fresh "a", "a14") ) 7 ;
    true
  with
  |_ -> false
;;