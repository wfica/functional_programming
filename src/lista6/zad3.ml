(*
Zadanie 3 (3p.)

Chcemy w Ocamlu zdefiniować funkcję sprintf znaną z języka C, tak by np. wyrażenie 

sprintf "Ala ma %d kot%s." : int -> string -> string
pozwalało zdefiniować funkcję 

fun n -> sprintf "Ala ma %d kot%s." n (if n = 1 then "a" else if 1 < n & n < 5 then "y" else "ów"

Na pierwszy rzut oka wydaje się, że rozwiązanie tego zadania wymaga typów zależnych, ponieważ 
typ funkcji sprintf zależy od jej pierwszego argumentu. Okazuje się jednak, że polimorfizm 
parametryczny wystarczy. Dla uproszczenia załóżmy, że format nie jest zadany przez wartość 
typu string (nie chcemy zajmować się parsowaniem), ale przez konkatenację następujących 
dyrektyw formatujących:
lit s - stała napisowa s
eol - koniec wiersza
inr - liczba typu int
flt - liczba typu float
str - napis typu string
Zakładając, że operatorem konkatenacji dyrektyw jest ++, powyższy przykład może być zapisany 
następująco: 

sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".") : int -> string -> string

Zdefiniuj funkcje lit, eol, inr, flt, str, ++ oraz funkcję sprintf.

Wskazówka: dyrektywy powinny być funkcjami transformującymi kontynuacje, a operator ++ to 
zwyczajne złożenie takich funkcji. Na przykład inr powinien mieć typ (string -> 'a) -> string 
-> (int -> 'a) (argumentem ma być kontynuacja oczekująca napisu, ale o nieokreślonym typie 
odpowiedzi, a wynikiem ma być kontynuacja oczekująca napisu, a następnie liczby całkowitej). 
Podobnie, typem eol ma być (string -> a) -> string -> a.
*)


let inr f s i = 
  f (s ^ string_of_int i )
;;

let flt f s fl = 
  f (s ^ string_of_float fl )
;;

let str f s st =
  f( s ^ st)
;;

let lit const f s =
  f (s ^ const)
;;  

let eol f s =
  f (s ^ "\n") 
;;

let (++) t1 t2 = function x -> t1 (t2 x);;

let sprintf format = format (fun x->x) ""

let test = (sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".") 5 "ow");;