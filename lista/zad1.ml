(*

Zadanie 1 (5p.)

Zdefiniuj operator stałopunktowy fix typu (('a->'b)->'a->'b)->'a->'b, który pozwoli 
na wyznaczanie punktu stałego funkcji typu ('a->'b)->'a->'b), a co za tym idzie na 
definiowanie rekurencyjnych funkcji bez użycia konstrukcji let rec. 
Np. silnię można wyrazić przy użyciu fix następująco: 

fix (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1)))

Nie używając rekursji (tj. konstrukcji let rec) zdefiniuj funkcję obliczającą silnię 
(użyj referencji). W podobny sposób zdefiniuj funkcję fix.
*)

let fac n = 
  let f1 = ref (fun n -> n) in 
  (
    f1 := 
      (
        fun n -> 
          if n > 0 
          then n * (!f1 (n-1)) 
          else 1
      );
    !f1 n
  ) 
;; 

fac 5 ;;
fac 10 ;; 

;;
let factorial n = 
  let f = ref (fun n -> n) in 
  ( 
    f := (fun n -> if n > 0 then n * (!f (n-1)) else 1  ); 
    !f n 
  )
;;

let fix f =
  let fixf = ref (fun x -> x) in 
  (
    fixf := (fun x -> f !fixf x);
    !fixf
  )
;;


let test () =
  let fixed_fun = fix (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1))) in 
  try 
    assert( factorial 0 = 1) ;
    assert( factorial 5 = 120);
    assert( fixed_fun 0 = 1) ;
    assert( fixed_fun 5 = 120);
    true
  with
  |_ -> false
;;


