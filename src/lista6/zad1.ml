(*

Zadanie 1 (4p.)

Definiujemy typ danych do reprezentacji drzew binarnych przechowujących wartości w liściach:

type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree

Dwa drzewa binarne typu t btree mają jednakowe brzegi (zakładamy, że obiekty typu t są 
porównywalne), jeśli listy utworzone przez odczytanie wartości w ich liściach od lewej do 
prawej są równe. Na przykład drzewa Node (Node (Leaf 1, Leaf 2), Leaf 3) i Node (Leaf 1, Node 
(Leaf 2, Leaf 3)) mają jednakowe brzegi równe [1; 2; 3]. 

(1p.) Napisz funkcję typu 'a btree -> 'a btree -> bool rozstrzygającą czy dwa drzewa mają 
jednakowe brzegi, bazując bezpośrednio na definicji i nie dbając o efektywność rozwiązania.
(3p.) Wykorzystując pojęcie odroczonego obliczenia, napisz efektywną i czysto funkcyjną wersję 
tej funkcji, tj. taką, która przerywa obliczenia w momencie napotkania pierwszej różnicy 
między brzegami drzew.

Wskazówka: należy odraczać trawersowanie prawego poddrzewa.
*)

open Core.Std;;
type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree ;;


let  gen_tree depth =
  let  rec _gen depth = 
    if depth <= 0 then
      Leaf(Random.int 10 )
    else Node(_gen @@ depth - 1 -  Random.int 2 , _gen @@ depth - 1 - Random.int 2)
  in _gen depth
;;
let t = gen_tree 3 ;;


let tree_to_stream tree =
  let rec convert acc =
    match acc with 
    | Leaf(x)::tl -> My_streams.SC(x, lazy( convert tl ) )
    | Node(l, r)::tl -> convert (l::r::tl)
    | [] -> My_streams.EoS
  in convert [tree]
;;

let rec boundary tree =
  match tree with 
  | Leaf(x) -> [x]
  | Node(t1, t2) -> boundary t1 @ boundary t2 
;;

let eq_boundaries t1 t2 = 
  boundary t1 = boundary t2 ;;


let eq_boundaries_lazy t1 t2 = 
  let rec _eq s1 s2 =
    match My_streams.peek s1, My_streams.peek s2 with
    | None, None -> true
    | _, None -> false
    | None, _ -> false
    | Some(x), Some(y) ->
      if x = y then _eq (My_streams.next s1 ) (My_streams.next s2 )
      else false
  in _eq (tree_to_stream t1) (tree_to_stream t2)
;;

let test_case expression number =
  try 
    match expression with 
    | lazy (a, b) -> assert (a = b)
  with 
  | e ->  printf "test number %i failed" number ;
    raise e 
;;

let test () =
  let t0 = Leaf(5) in
  let t1 = Leaf(5) in
  let t2 = Node(Leaf(1), Node(Leaf(2), Leaf(3))) in
  let t3 = Node(Node(Leaf(1), Leaf(2)), Leaf(3)) in
  let t4 = Node(Node(Leaf(3), Leaf(3)), Leaf(3)) in 
  try 
    test_case (lazy( eq_boundaries_lazy t2 t4, false )) 0;
    test_case (lazy( eq_boundaries_lazy t0 t1, true )) 1;
    test_case (lazy( eq_boundaries_lazy t0 t2, false )) 2;
    test_case (lazy( eq_boundaries_lazy t3 t2, true )) 3;
    true
  with 
  | _ -> false
;;

test () ;;