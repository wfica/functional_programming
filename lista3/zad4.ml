(*
zad4
Niech macierz kwadratowa n x n będzie reprezentowana wierszami jako lista 
list. Wykonaj następujące polecenia opierając swoje rozwiązania na 
wykorzystaniu funkci bibliotecznych z modułu List.

Napisz funkcję sprawdzającą, czy dana lista jest poprawną reprezentacją 
macierzy kwadratowej.
Napisz funkcję, która dla zadanej macierzy kwadratowej i liczby naturalnej n 
wyznacza n-tą kolumnę macierzy.
Wykorzystaj funkcję z poprzedniego punktu do napisania funkcji transpozycji 
macierzy, np. transpozycja macierzy [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]] jest 
reprezentowana jako lista [[1.;4.;7.];[2.;5.;8.];[3.;6.;9.]].
Napisz funkcję zip, która dla danych dwóch list równej długości tworzy 
listę złożoną z par elementów obu list znajdujących się na tych samych 
pozycjach, np. zip [1.;2.;3.] ["a";"b";"c"] = [(1.,"a");(2.,"b");(3.,"c")].
Korzystając z funkcji zip, napisz funkcję zipf, która dla danych dwóch list 
typów 'a list i 'b list i funkcji dwuargumentowej f typu 'a -> 'b -> 'c tworzy 
listę złożoną z wartości funkcji f na argumentach z obu list położonych 
na tych samych pozycjach, np. zipf ( +. ) [1.;2.;3.] [4.;5.;6.] = [5.;7.;9.].
Wykorzystując funkcję zipf napisz funkcję mult_vec, która oblicza iloczyn 
zadanego wektora i zadanej macierzy, np. mult_vec [1.;2.] [[2.;0.];[4.;5.]] = 
[10.;10.].
Korzystając z funkcji mult_vec napisz funkcję mnożenia dwóch macierzy 
kwadratowych tego samego rozmiaru.
*)
open Core.Std;;
open QCheck;; 

let is_square ll =
  let len = List.length ll in
  List.for_all ll ~f:(fun l -> List.length l = len)
;;

let nth_column ll n =
  List.fold_right ll ~f: (fun l acc -> List.nth_exn l n :: acc) ~init: []
;;

let transpose ll =
  let range = List.range 0 (List.length ll)   in
  List.fold_right range ~f: (fun n acc -> nth_column ll n :: acc ) ~init: []
;; 

let zip l1 l2 =
  List.map2_exn l1 l2 ~f: ( fun a b -> a, b)
;;

let zipf ~f l1 l2 =
  List.map2_exn l1 l2 ~f: (fun a b -> f a b)
;;

let mult_vec v ~matrix =
  transpose matrix |> 
  List.map ~f:(fun col -> List.fold2_exn col v ~init: 0. ~f:(fun acc a b -> a *. b +. acc) )
;;

let mult_mat m1 m2 =
  List.map m1 ~f:(mult_vec ~matrix:m2 )
;;

let test() =
  let matrix = [[1.;2.;3.];
                [4.;5.;6.];
                [7.;8.;9.]]
  and col0 = [1.;4.;7.] 
  and col1 = [2.;5.;8.]
  in
  try
    assert ( is_square [] = true) ;
    assert ( is_square [[1;2];[1;4]] = true ) ;
    assert ( is_square [[1;2];[0]] = false ) ;

    assert (nth_column matrix 0 = col0) ;
    assert (nth_column matrix 1 = col1) ;

    assert ( transpose matrix = [[1.;4.;7.];[2.;5.;8.];[3.;6.;9.]] ) ;

    assert  (zip [1.;2.;3.] ["a";"b";"c"] = [(1.,"a");(2.,"b");(3.,"c")] );

    assert (zipf ~f:( +. ) [1.;2.;3.] [4.;5.;6.] = [5.;7.;9.] ) ;

    assert ( mult_mat matrix (transpose matrix ) = [[14.; 32.; 50.]; [32.; 77.; 122.]; [50.; 122.; 194.]]) ;
    true;
  with 
  | _ -> false;
;;
