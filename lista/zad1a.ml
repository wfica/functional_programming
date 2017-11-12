(*

Zadanie 1 (4p.)

Zdefiniuj typ do reprezentacji leniwych nieskończonych ciągów elementów dowolnego typu 
(strumieni) tak, jak na wykładzie. Następnie:

	zdefiniuj strumień przybliżający wartość liczby π z rosnącą dokładnością, korzystając 
	ze wzoru Leibniza:

		π/4 = 1 - 1/3 + 1/5 - 1/7 + ...

	napisz funkcję przekształcającą dowolny strumień

		x1,x2,x3,...

	w strumień postaci

		f x1 x2 x3, f x2 x3 x4, f x3 x4 x5,... 

	dla dowolnej funkcji f
	
	korzystając z powyższych definicji i transformacji Eulera:

		F x y z = z - (y-z)2/(x-2y+z)

	utwórz nowy strumień, szybciej zdążający do liczby π.
	
Napisz te same definicje używając konstrukcji lazy i force z modułu Lazy. Dlaczego lazy jest 
specjalną konstrukcją języka, a nie funkcją? 
*)

open Core.Std;;


type 'a llist = LNil | LC of 'a * (unit -> 'a llist);;

let lhd ll = 
  match ll with
  | LNil -> failwith "lhd of empty llist"
  | LC(x, _) -> x
;;

let ltl ll =
  match ll with
  | LNil -> failwith "ltl of empty llist"
  | LC(_, f) -> f () 
;;

let sgn_float x = if x >= 0. then 1. else -1.;;

let next_leibniz k = 
  let sgn = sgn_float k in
  -1. *. sgn /. (sgn /. k +. 2.)
;;

let  rec leibniz_from k = LC(k, fun () -> leibniz_from ( next_leibniz k ) )

let leibniz = leibniz_from 1.

let sum_pref_stream stream =
  let rec _spref_stream s acc =
    match s with 
    | LNil -> LNil
    | LC(hd, f) -> LC(acc +. hd, fun () -> _spref_stream (f()) (acc +. hd ) )
  in _spref_stream stream 0.
;;

let leibniz_approx = sum_pref_stream  leibniz;;

let rec ltake ll n =
  match ll, n with
  | LNil, _ -> []
  | _, 0 -> []
  | LC(hd, f), _ -> hd :: ltake (f ()) (n-1) 
;;

let rec ltake_with_tail ll n =
  match ll, n with
  | LNil, _ -> ([], LNil)
  | xs, 0  -> ([], xs)
  | LC(hd, f), _ -> 
    let (l, tl) = ltake_with_tail ( f () )  (n-1)
    in (hd::l, tl)
;;



let transform_stream ll f =
  let rec _transform ll f =
    let tail = ltake_with_tail ll 1  |> snd 
    and args = ltake_with_tail ll 3  |> fst in
    LC(f args, fun () -> _transform tail f )
  in
  _transform ll (fun [a; b; c] -> f a b c )
;;


let euler_transform x y z = z -. (y -. z) *. (y -. z) /. (x -. 2. *. y +. z);;

let faster_conv = transform_stream leibniz_approx euler_transform;;

let test () = 
  let pi_4 = 0.78539816339 in 
  let approx1 =  ltake  leibniz_approx 10000 in
  let approx2 = ltake faster_conv 10000 in
  let args = [0; 9; 99; 999; 9999] in
  [ List.map ~f:(fun i -> List.nth_exn approx1 i -. pi_4 ) args ;
    List.map ~f:(fun i -> List.nth_exn approx2 i -. pi_4 ) args ]

;;


(*--------------------------------------------------------------*)

