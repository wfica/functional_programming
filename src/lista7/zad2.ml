(*

Zadanie 2 (3p.)

Rozważmy modyfikowalne listy zdefiniowane następująco: 

type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

Zaimplementuj konkatenację list typu 'a list_mutable na dwa sposoby:
funkcja concat_copy buduje listę wynikową kopiując pierwszy argument;
funkcja concat_share buduje listę wynikową bez kopiowania argumentów.
;; 
*)

type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

let rec len l =
  match l with
  | LMnil -> 0
  |LMcons(_, tl) -> 1 + len !tl
;;

let rec concat_copy l1 l2 = 
  match l1 with
  | LMnil -> l2
  | LMcons(e, tl) -> LMcons(e, ref @@ concat_copy !tl l2)
;;


let concat_share l1 l2 =
  let rec aux l1 l2 =
    match !l1 with 
    | LMnil -> l1 := l2 
    | LMcons(_, tl) -> aux tl l2
  in aux l1 l2 
;;

let test_case (lazy (a, b)) number =
  try 
    assert (a = b)
  with 
  | e ->  Core.Std.printf "test number %i failed" number ;
    raise e 
;;


let test () =
  let l1 = LMcons(1, ref @@ LMcons(2, ref LMnil)) in 
  let l2 = LMcons(3, ref LMnil) in 
  let l3 = concat_copy l1 l2 in 
  let l4_ = ref @@ LMcons(11, ref @@ LMcons(22, ref LMnil)) in
  concat_share l4_ l3 ;
  try 
    test_case( lazy(len l3, 3) ) 0;
    test_case( lazy(len !l4_, 5) ) 1;
    true
  with
  |_ -> false
;;


