(*
Zadanie 4 (3p.)

Funkcja jest napisana w stylu przekazywania kontynuacji (continuation-passing style, CPS), jeżeli 
przyjmuje dodatkowy argument funkcyjny zwany kontynuacją, który reprezentuje całą resztę 
obliczeń jakie mają zostać przeprowadzone po powrocie z tej funkcji. W konsekwencji, funkcje w 
CPS-ie zwracają wynik wywołując swoją kontynuację, a wszystkie wywołania w programie w CPS-ie 
są ogonowe. Na przykład, funkcja licząca silnię napisana w CPS-ie wygląda następująco: 

let rec fact_cps n k = 
  if n = 0 
  then k 1 
  else fact_cps (n-1) (fun v -> k (n*v))
  
Kontynuacja początkowa przekazana funkcji fact_cps mówi co zrobić z wynikiem obliczenia silni 
zadanej liczby. Typowe wywołanie funkcji fact_cps dostaje identyczność jako kontynuację 
początkową (gdy silnia jest obliczona, wystarczy ją zwrócić):

let fact n = fact_cps n (fun v -> v)

Dla drzew binarnych z zadania 2, napisz funkcję prod : int btree -> int, która liczy iloczyn 
wszystkich wartości w drzewie. Zapisz tę funkcję w CPS-ie, a następnie zmodyfikuj otrzymaną 
funkcję tak by w przypadku napotkania wartości 0 funkcja wykonała bezpośredni skok do miejsca 
swego wywołania, bez krokowego powracania z rekursji.
*)
open Core.Std;;

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let rec fact_cps n k = 
  if n = 0 
  then k 1 
  else fact_cps (n-1) (fun v -> k (n*v))
;;

let rec prod_cps tree = fun k -> 
  match tree with
  | Leaf -> k 1
  | Node(left, v, right) -> 
    prod_cps left @@
    fun left_prod -> prod_cps right @@
    fun right_prog -> k @@ v * left_prod * right_prog
;;

let prod tree = prod_cps tree (fun x -> x)
;;


exception Zero;;

let rec prod_cps_exn tree = fun k -> 
  match tree with
  | Leaf -> k 1
  | Node(_, 0, _) -> 0
  | Node(left, v, right) -> 
    prod_cps left @@
    fun left_prod -> prod_cps right @@
    fun right_prog -> k @@ v * left_prod * right_prog
;;

let prod2 tree =
  try 
    prod_cps_exn tree (fun x -> x)
  with
  | Zero -> 0
;;

let test () =
  let t1 = Node (Leaf, 5, Leaf) in 
  let t2 = Node ( Node(Leaf, 5, Leaf), 1, Node(Leaf, 7, Node(Leaf, 8, Leaf))) in 
  let t3 = Node (Node( Node(Leaf, 3, Leaf),2, Leaf), 1, Leaf) in
  let t4 = Node(t3, 7, t2) in
  let t5 = Node( t4, 0, t4) in
  let t6 = Node(t5, 1, t4) in
  let trees = [t1; t2; t3; t4; t5; t6] in
  let results = [5; 280; 6; 11760; 0; 0] in
  try
    assert( List.for_all2_exn trees results ~f:(fun x y -> prod x = y) ) ;
    assert( List.for_all2_exn trees results ~f:(fun x y -> prod2 x = y) ) ;
    true;
  with 
  | _ -> false
;;


(*
let rec add_cps x y k = k @@ x + y 

let rec square_cps x k = k @@ x * x 

let rec pit_cps x y = 
  fun k -> square_cps x 
  (fun x_squared -> square_cps y 
  (fun y_squared -> add_cps x_squared y_squared  k ))
*)


let mult x = fun y -> x * y ;;





