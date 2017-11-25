

(*
Zadanie 4 (3p.)

Problem Józefa definiuje się następująco (Cormen et al. str. 340). Niech n osób stoi w
okręgu oraz niech dana będzie liczba m i n. Rozpoczynając od wskazanej osoby,
przebiegamy po okręgu, usuwając co m-tą osobę. Po usunięciu każdej kolejnej osoby
odliczanie odbywa się w nowo powstałym okręgu. Proces ten postępuje, aż zostaną
usunięte wszystkie osoby. Porządek, w którym osoby stojące początkowo w okręgu są z
niego usuwane, definiuje permutację Józefa typu (n,m) liczb 1, 2, ... ,n. Na przykład
permutacją Józefa typu (7,3) jest <3,6,2,7,5,1,4,7>. Napisz funkcję typu int -> int -> int
list, która dla danych n oraz m zwraca listę z permutacja Józefa typu (n,m). Należy
wykorzystać listę cykliczną.
*)
;;

type 'a lnode = {item: 'a; mutable next: 'a lnode};;
let mk_circular_list e =
  let rec x = {item=e; next=x} 
  in x;;
let insert_head e l =
  let x = {item=e; next=l.next} 
  in l.next <- x ; l;;
let insert_tail e l =
  let x = {item=e; next=l.next} 
  in l.next <- x; x ;;
let first l = l.next.item;;
let last l = l.item;;
let elim_head l = l.next <- (l.next).next; l ;;

let make_list n =
  let open Core.Std in 
  let l = mk_circular_list 1 in
  List.range 2 (n+1) |>
  List.rev |>
  List.fold_right ~f:(fun i acc -> insert_tail i acc) ~init:l 
;;

let shift_list l  m =
  let res = ref l in 
  for i=1 to m do 
    res:= !res.next 
  done; 
  !res ;;


let perm_jozef n m  = 
  let res = ref [] 
  and lc = ref @@ make_list n in   
  for i=1 to n do
    lc := shift_list !lc (m-1) ;
    res := (first !lc)::!res ;
    lc := elim_head !lc
  done;
  List.rev !res;;

 perm_jozef 7 3 ;;

 let test_case (lazy (a, b)) number =
  try 
    assert (a = b)
  with 
  | e ->  Core.Std.printf "test number %i failed" number ;
    raise e 
;;

let test () =
  try 
    test_case( lazy(perm_jozef 7 3, [3; 6; 2; 7; 5; 1; 4])) 0;
    test_case( lazy(perm_jozef 1 2, [1])) 1;
    test_case( lazy(perm_jozef 7 1, [1;2;3;4;5;6;7])) 2;
    true
  with
  |_ -> false
;;

