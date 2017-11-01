(*
zad5

Napisz funkcję, która dla zadanej permutacji elementów dowolnego typu 'a, na 
którym zdefiniowany jest pewien porządek liniowy, znajduje kolejną, w 
porządku leksykograficznym, permutację tych samych elementów, np. dla 
permutacji (1,2,4,3) funkcja powinna zwrócić permutację (1,3,2,4), a dla 
permutacji (a,c,b) wynikiem powinna być permutacja (b,a,c). W przypadku, w 
którym zadana permutacja jest największa, funkcja powinna zwracać 
permutację najmniejszą. Do reprezentowania permutacji użyj list, ale w 
porządku odwróconym, tj. w przykładzie powyżej funkcja dla argumentu 
[3;4;2;1] powinna dać odpowiedź [4;2;3;1], a dla ['b';'c';'a'] - odpowiedź 
['c';'a';'b']. Wykorzystaj tę funkcję do napisania funkcji generującej 
wszystkie permutacje danej listy.

*)
open Core.Std;;

let zip_shorter l1 l2 =
  let rec _zip l1 l2 acc =
    match l1, l2 with
    | h1::t1, h2::t2 -> _zip t1 t2 ((h1, h2)::acc)
    | _, _ -> List.rev acc
  in _zip l1 l2 []
;;

let longest_incr_pref l =
    let zipped = zip_shorter l @@ List.drop l 1  in
    let len = List.length @@ List.take_while zipped ~f:(fun a -> fst a < snd a )  in
    len + 1
;;

let next_perm l =
  let pref = longest_incr_pref l in
  let p, s = List.split_n l pref in
  match s with
  | [] -> List.rev p
  | hd::tl -> 
    let p1, p2 = List.split_while p ~f:((>) hd) in
    match p2 with 
    | h2::t2 -> List.append (List.append (List.rev t2) (hd::List.rev p1) ) (h2::tl)
    | _ -> [] (* unreachable *)
;;

let all_perms l = 
  let rec _all_perms l curr acc =
    let next = next_perm curr in
    if next = l then acc
    else _all_perms l next (next::acc)
  in _all_perms l l [l]
;;

let test () =
  try 
    assert ( all_perms [3;2;1;4] = 
    [[1; 2; 4; 3]; [2; 1; 4; 3]; [1; 4; 2; 3]; [4; 1; 2; 3]; [2; 4; 1; 3];
     [4; 2; 1; 3]; [1; 3; 4; 2]; [3; 1; 4; 2]; [1; 4; 3; 2]; [4; 1; 3; 2];
     [3; 4; 1; 2]; [4; 3; 1; 2]; [2; 3; 4; 1]; [3; 2; 4; 1]; [2; 4; 3; 1];
     [4; 2; 3; 1]; [3; 4; 2; 1]; [4; 3; 2; 1]; [1; 2; 3; 4]; [2; 1; 3; 4];
     [1; 3; 2; 4]; [3; 1; 2; 4]; [2; 3; 1; 4]; [3; 2; 1; 4]]) ;
    assert ( all_perms [1] = [[1]]) ;
    true;
  with 
  | _ -> false;
;;