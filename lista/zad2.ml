(*
Zadanie 2 (3p.)

Napisz możliwie efektywne funkcje przejścia wszerz oraz przejścia w głąb 
(w porządku preorder) dla obu reprezentacji drzew wielokierunkowych podanych 
na wykładzie (zadanie 5 z listy kontrolnej do wykładu 4). Funkcje powinny 
zwracać listę wartości znalezionych w drzewie w kolejności odwiedzania jego 
wierzchołków.
*)

open Core.Std;;

type 'a mtree = MNode of 'a * 'a forest
and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest;;

let list_of_forest forest = 
  let rec f2l forest acc =
    match forest with
    | EmptyForest -> List.rev acc
    | Forest(tree, f) -> f2l f (tree::acc)
  in
  f2l forest []
;;


let dfs1 tree =
  let rec _dfs visited stack =
    match stack with
    | [] -> List.rev visited
    | MNode(v, childern)::tl -> _dfs (v::visited) ( list_of_forest childern @ tl)
  in _dfs [] [tree]
;;

let bfs1 tree =
  let rec _bfs visited queue =
    match queue with
    | [] -> List.rev visited
    | MNode(v, childern)::tl -> _bfs (v::visited) (  tl @ list_of_forest childern )
  in _bfs [] [tree]
;;

let test1 () = 
  (* big  tree:
          1
      /   |    \
    2     3     4
   /    / | \
  5    6  7  8
          |   
          9
  *)
  let big_tree = 
    MNode(1, 
      Forest(MNode(2, 
        Forest(MNode(5, EmptyForest), EmptyForest)), 
      Forest(
        MNode(3, 
          Forest(MNode(6, EmptyForest), 
          Forest(MNode(7, 
            Forest(MNode(9, EmptyForest), EmptyForest)), 
          Forest(MNode(8, EmptyForest), EmptyForest)) )  )
        , 
      Forest(MNode(4, EmptyForest), EmptyForest)) )  )
  in
  try 
    assert ( dfs1 big_tree = [1; 2; 5; 3; 6; 7; 9; 8; 4]) ;
    assert ( dfs1 @@ MNode(1, EmptyForest) = [1] ) ;
    assert ( bfs1 big_tree = [1; 2; 3; 4; 5; 6; 7; 8; 9]) ;
    assert ( bfs1 @@ MNode(1, EmptyForest) = [1] ) ;
    true
  with
  | _ -> false
;;


type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;

let dfs2 tree =
  let rec _dfs vst stack =
    match stack with 
    | [] -> List.rev vst
    | MTree(v, l)::tl -> _dfs (v::vst) (l @ tl)
  in 
  _dfs [] [tree]
;;

let bfs2 tree =
  let rec _bfs vst queue =
    match queue with 
    | [] -> List.rev vst
    | MTree(v, l)::tl -> _bfs (v::vst) (tl @ l)
  in 
  _bfs [] [tree]
;;

let test2 () = 
  (* big  tree:
          1
      /   |    \
    2     3     4
   /    / | \
  5    6  7  8
          |   
          9
  *)
  let big_tree = 
    MTree(1, [
      MTree(2, [
        MTree(5, [])]);
      MTree(3, [
        MTree(6,[]);
        MTree(7, [
          MTree(9, [])]);
        MTree(8, [])]);
      MTree(4, [])]) 
  in
  try 
    assert ( dfs2 big_tree = [1; 2; 5; 3; 6; 7; 9; 8; 4]) ;
    assert ( dfs2 @@ MTree(1, []) = [1] ) ;
    assert ( bfs2 big_tree = [1; 2; 3; 4; 5; 6; 7; 8; 9]) ;
    assert ( bfs2 @@ MTree(1, []) = [1] ) ;
    true
  with
  | _ -> false
;;

let test () = test1 () && test2 () ;;
