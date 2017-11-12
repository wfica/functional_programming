(*

Zadanie 1 (2p.)

Rozważmy typ danych dla drzew binarnych, zdefiniowany następująco: 

type α btree = Leaf | Node of α btree * α * α btree

Mówimy, że drzewo jest zbalansowane, jeśli dla każdego węzła v liczby węz
łów w lewym i prawym poddrzewie zakorzenionym w v różnią się co najwyżej 
o 1.

(a) Napisz efektywną funkcję sprawdzającą czy dane drzewo jest zbalansowane.
(b) Napisz funkcję, która dla zadanej listy etykiet tworzy zbalansowane drzewo,
    dla którego tę listę można otrzymać przechodząc je w porządku preorder.

*)
open Core.Std;;

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;


let t1 = Node (Leaf, 5, Leaf);;
let t2 = Node ( Node(Leaf, 5, Leaf), 1, Node(Leaf, 7, Node(Leaf, 8, Leaf)));;
let t3 = Node (Node( Node(Leaf, 3, Leaf),2, Leaf), 1, Leaf) ;;


let balanced tree =
  let rec _balanced tree = 
    match tree with
    | Leaf -> Some 0
    | Node(left, _, right) -> 
      match _balanced left with
      | None -> None
      | Some left_v -> 
        match _balanced right with
        | None -> None
        | Some right_v -> if abs (left_v - right_v) > 1 then None else Some (left_v + right_v + 1)
  in
  match _balanced tree with
  | None -> false
  | _ -> true
;;

let tree_of_list l =
  let rec _l2t l n = 
    match l with 
    | [] -> Leaf
    | hd :: tl-> let m = n -1 in
      Node(_l2t (List.take tl (m/2) ) (m/2) , hd, _l2t (List.drop tl (m/2)) (m - m/2) )
  in _l2t l (List.length l )
;;


let test () =
  try 
    let t1 = Node (Leaf, 5, Leaf) 
    and t2 = Node ( Node(Leaf, 5, Leaf), 1, Node(Leaf, 7, Node(Leaf, 8, Leaf)))
    and t3 = Node (Node( Node(Leaf, 3, Leaf),2, Leaf), 1, Leaf) 
    in 
    assert( balanced Leaf = true );
    assert( balanced t1 = true );
    assert( balanced t2 = true );
    assert( balanced t3 = false );

    assert( tree_of_list [] = Leaf) ;
    assert( tree_of_list [1;2;3;4;5;6;7;8] = 
      Node (Node (Node (Leaf, 3, Leaf), 2, Node (Leaf, 4, Leaf)), 1,
      Node (Node (Leaf, 6, Leaf), 5, Node (Leaf, 7, Node (Leaf, 8, Leaf))))) ;

    assert( tree_of_list [1] = Node(Leaf, 1, Leaf)) ;
    true 
  with 
  | _ -> false
;;
