(*
Zadanie 2 (4p.)

Definiujemy typ danych do reprezentacji drzew binarnych przechowujących wartości zarówno w 
węzłach jak i w liściach: 

type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree

(1p.) Napisz funkcję numerującą węzły i liście drzewa binarnego w kolejności przechodzenia 
go w głąb (preorder). Na przykład, tak ponumerowaną wersją drzewa Node (Node (Leaf 'a', 'b', 
Leaf 'c'), 'd', Leaf 'e') jest Node (Node (Leaf 3, 2, Leaf 4), 1, Leaf 5).
(3p.) Napisz funkcję numerującą węzły i liście drzewa binarnego w kolejności przechodzenia 
go wszerz. Na przykład, tak ponumerowaną wersją drzewa Node (Node (Leaf 'a', 'b', Leaf 'c'), 
'd', Leaf 'e') jest Node (Node (Leaf 4, 2, Leaf 5), 1, Leaf 3).
Wskazówka: lasy numeruje się łatwiej niż drzewa.

*)
open Core.Std;;
type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree;;



let preorder tree =
  let rec dfs tree cnt = 
    match tree with
    | Leaf(_) -> Leaf(cnt), (cnt+1)
    | Node(l, _, r) ->
      let l_, cnt_  = dfs l (cnt + 1) in
      let r_, cnt__ = dfs r cnt_  in
      Node(l_, cnt, r_), cnt__
  in dfs tree 1 
;;

let bfs_order tree =
  let rec zip_layers layer rec_result cnt  =
    match layer with 
    | [] -> []
    | hd::tl -> 
      match hd, rec_result with 
      | Leaf(_), _ -> Leaf(cnt) :: zip_layers tl rec_result (cnt+1)
      | Node(_), e1::e2::rest -> Node(e1, cnt, e2) :: zip_layers tl rest (cnt+1)
      | _, _ -> failwith "unreachable"
  in 
  let rec bfs layer cnt =
    if layer = [] then [] else
    let next_layer = List.concat_map layer ~f:(fun node -> match node with | Leaf _ -> [] | Node (l, _, r)-> [l; r]) in 
    let rec_result = bfs next_layer (cnt + List.length layer ) in 
    zip_layers layer rec_result cnt in 
  bfs [tree] 1
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
  let in0 = Node (Node (Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e') in
  let out0 = Node (Node (Leaf 3, 2, Leaf 4), 1, Leaf 5) in
  let in1 = Leaf('a') in
  let out1 = Leaf(1) in
  let in2 = Node (Node (Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e') in 
  let out2 = Node (Node (Leaf 4, 2, Leaf 5), 1, Leaf 3) in 
  try 
    test_case (lazy( preorder in0 |> fst , out0 )) 0;
    test_case (lazy( preorder in1 |> fst , out1 )) 1;
    test_case (lazy( bfs_order in2 |> List.hd_exn, out2 ) ) 2;
    test_case (lazy( bfs_order in1 |> List.hd_exn, out1 ) ) 3;
    true
  with 
  | _ -> false
;;
