(*
Zadanie 2 (8p.)

Mamy ciąg szklanek o znanych pojemnościach, a także kran i zlew. W każdym ruchu możemy 
wykonać jedną z trzech czynności:

napełnić jedną ze szklanek wodą z kranu (FILL),
opróżnić jedną ze szklanek do zlewu (DRAIN),
przelać wodę z jednej ze szklanek do innej (TRANSFER).

Stan takiego układu szklanek możemy zapisać przy użyciu dwóch list reprezentujących 
odpowiednio pojemności szklanek i ilość wody znajdującą się w każdej z nich. Przykładowo, 
para ([4; 9], [4; 6]) oznacza że dysponujemy dwiema szklankami o pojemnościach 4 i 9, oraz że 
pierwsza z nich jest pełna, zaś w drugiej znajduje się 6 jednostek wody. W takim przypadku 
efekty poniższych ruchów następująco zmieniają tę zawartość szklanek (czyli drugą z 
powyższych list; zwróć uwagę że można napełnić pełną szklankę):

FILL 1 → [4; 9]
FILL 0 → [4; 6]
DRAIN 0 → [0; 6]
TRANSFER (0, 1) → [1; 9].

Dla danego zestawu szklanek i danej objętości wody, rozwiązaniem nazywamy ciąg ruchów, który 
prowadzi do uzyskania w dowolnej ze szklanek zadanej objętości wody. Przykładowo, dla szklanek 
[4, 9] i objętości 5, rozwiązaniem jest ciąg [FILL 1, TRANSFER (1, 0)] (a także wiele innych, 
redundantnych ciągów).

Użyj leniwych list aby zdefiniować funkcję nsols : (int list * int) -> int -> move list list, 
taką że nsols (glasses, volume) n zwróci listę n najkrótszych rozwiązań problemu zadanego 
przez glasses i volume (typ danych move powinien reprezentować pojedynczy ruch). W przypadku gdy 
rozwiązanie nie istnieje, program może się zapętlić.

Wskazówka: 
Ponieważ dopuszczamy redundantne ciągi (np. przelewanie z pustego w próżne), a 
także możemy się zapętlić gdy rozwiązanie nie istnieje, nie potrzeba sprawdzać czy daną 
konfigurację można osiągnąć w inny (potencjalnie lepszy) sposób.

Uwaga: 
Dzięki użyciu leniwych list, możliwa jest implementacja, w której w programie let f = 
nsols (g, v) in (f n; f n) drugie wywołanie f n działa w czasie O(n).
*)
open Core.Std;;
open Llist;;
type move = Fill of int | Drain of int | Transfer of int * int 
;;


let generate_fills state glasses =
	let state_val = fst state in 
	let zipped = List.zip_exn state_val glasses in 
	List.foldi zipped ~init:[] ~f:(fun i acc (s, g) -> 
	if s = g 
	then acc 
	else((List.take state_val i @ [g] @ List.drop state_val (i+1) ), Fill i :: snd state )::acc   )
;;

let generate_drains state =
	let state_val = fst state in  
	List.foldi state_val ~init:[] ~f:(fun i acc s -> 
	if s = 0
	then acc 
	else((List.take state_val i @ [0] @ List.drop state_val (i+1) ), Drain i :: snd state )::acc   )
;;

let generate_pairs n =
  List.range 0 n 
  |> List.concat_map ~f:(fun i -> List.range 0 i @ List.range (i+1) n  |> 
                                  List.map ~f:(fun e -> (i, e) ) )
;;

let take_interval l a b =
  let rec _take_interval l a b = 
    match a, l with 
    | 0, _ -> List.take l b
    | _, _::tl -> _take_interval tl (a-1) (b-1)
    | _, _ -> failwith "take_interval failure" 
  in _take_interval l (max 0 a) b 
;;

let transfer state glasses i j n =
  let part = take_interval state  
  and s_i = List.nth_exn state i 
  and s_j = List.nth_exn state j 
  and g_j = List.nth_exn glasses j in
  let new_val = min (s_j + s_i) g_j in
  if i < j 
  then part 0 i @ [s_i - new_val + s_j] @ part (i+1) j @ [new_val] @ part (j+1) n 
  else part 0 j @ [new_val] @ part (j+1) i @ [s_i - new_val + s_j] @ part (i+1) n 
;;


let generate_transfers state glasses n = 
  generate_pairs n  |>
  List.map ~f:(fun (i, j) -> (transfer (fst state) glasses i j n, 
                              Transfer (i,j):: (snd state)  ) )
;;

let generate_states state glasses n =
  generate_drains state @ 
  generate_fills state glasses @
  generate_transfers state glasses n
;; 

let zeros n = 
  let rec _repeat value n acc =
    match n with 
    | 0 -> acc
    | _ -> _repeat value (n-1) (value::acc)
  in _repeat 0 n []
;;


let find_all_moves (glasses, volume) =
  let rec bfs queue n =
    match Queue.dequeue queue with
    | None -> LNil
    | Some state -> 
      let new_states = generate_states state glasses n in 
      let _ = Queue.enqueue_all queue new_states in
      match List.find new_states ~f:(fun (s, _) -> 
          List.exists s ~f:(fun elem -> elem = volume) ) with
      |	None -> bfs queue n
      | Some result -> LC( result, lazy(  bfs queue n ) )
  in 
  let n = List.length glasses in
  let queue = Queue.create () in 
  let _ = Queue.enqueue queue (zeros n, []) in
  bfs queue n 
;;

let nsols (glasses, volume) =
	let possibilities = find_all_moves (glasses, volume) in 
	function n -> ltake possibilities  n
;;



let test () =
	let f = nsols ([4;9], 5) in
	let _ = f 5 in
	let _ = f 6 in 
	true
;;

test () ;;