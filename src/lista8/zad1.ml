(*
 Zadanie 1 (2p.)

Jaki powinien być wynik ewaluacji poniższego wyrażenia?

let k = 
  (let i = print_newline() 
   in fun q -> fun i -> "") ()
in 0 
Sprawdź, czy w twojej wersji Ocamla oba kompilatory ocamlc i ocamlopt generują poprawny 
kod dla tego przykładu.

Przeczytaj artykuł Effect-driven QuickChecking of compilers 
[http://janmidtgaard.dk/papers/Midtgaard-al%3aICFP17-full.pdf]
(a przynajmniej sekcje 1,4,7). *)



let k = 
  (let i = print_newline() 
   in fun q -> fun i -> "") () 
in 0 ;;

let k2 = 
  let f = (let i = print_newline() 
           in fun q -> fun i -> "")  
  in ( f () )
in 0;; 

