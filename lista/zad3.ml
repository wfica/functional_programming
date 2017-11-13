(*
Zadanie 3 (8p.)

Kodowanie Huffmana jest prostą metodą bezstratnej kompresji. Załóżmy, że dana jest lista 
asocjacyjna złożona z par (symbol, częstość występowania). Zaimplementuj statyczny algorytm 
kodowania i dekodowania napisów (czyli ciągów symboli z listy) według następujących zasad:

Drzewo kodowe jest drzewem binarnym, w którego liściach znajdują się symbole wraz z ich 
częstościami, a w każdym węźle znajduje się suma częstości poddrzew lewego i prawego. 
Zdefiniuj typ takich drzew htree.

Algorytm budowy drzewa Huffmana jest następujący:
@ Mając daną listę drzew, wybieramy z niej dwa drzewa o najmniejszych częstościach (ten wybór 
nie musi być jednoznaczny).
@ Usuwamy te drzewa z listy, łączymy je w jedno drzewo i wstawiamy do listy.
@ Kontynuujemy aż do otrzymania listy jednoelementowej (czyli drzewa kodowego).
@ Początkowa lista składa się z liści otrzymanych z listy częstości.
@ Łączenie dwóch poddrzew polega na utworzeniu nowego drzewa o częstości będącej sumą 
częstości obu poddrzew.

Zaimplementuj funkcję mkHTree : (char * int) list -> htree implementującą ten algorytm.

@ Kodem symbolu jest zapis ścieżki od korzenia drzewa kodowego do liścia zawierającego ten 
symbol, gdzie ścieżka jest reprezentowana jako ciąg zer i jedynek, w którym 0 oznacza zejście 
do lewego poddrzewa, a 1 zejście do prawego poddrzewa (lub na odwrót, ale jednakowo dla 
wszystkich symboli). Kod napisu to ciąg kodów wszystkich symboli tego napisu. Dla wybranego typu 
strumieni α stream zdefiniuj funkcję encode : htree -> char stream -> char stream, gdzie znaki w 
strumieniu wyjściowym oznaczają bajty upakowane bitami zakodowanego strumienia (biblioteka OCamla 
nie dostarcza domyślnie typów danych odpowiadających słowom; do stworzenia "znaków" użyj 
funkcji Char.chr).
@ Dekodowanie jest procesem odwrotnym: dla danego ciągu zer i jedynek kodującego pewien napis, 
wędrujemy po drzewie według kierunków zapisanych w kodzie, zaczynając od korzenia. Gdy dotrzemy 
do liścia, odczytujemy umieszczony w nim symbol i odrzucamy wykorzystany dotąd fragment kodu. 
Następnie dla pozostałego fragmentu kodu wracamy do korzenia i odszukujemy kolejne symbole, aż 
do wyczerpania kodu. Zdefiniuj funkcję decode : htree -> char stream -> char stream odwrotną do 
funkcji encode. Wartości liczbowe znaków odzyskaj używając Char.code.

Na bazie swojej implementacji zaprogramuj funkcję kompresującą oraz funkcję dekompresującą 
pliki tekstowe. Funkcja kompresująca powinna na podstawie zawartości zadanego pliku wejściowego 
wyliczyć częstości występowania symboli, utworzyć drzewo Huffmana, a następnie zakodować 
plik wejściowy do pliku wyjściowego używając funkcji encode. Funkcja powinna też zwrócić 
skonstruowane drzewo Huffmana. Funkcja dekompresująca powinna na podstawie zawartości zadanego 
pliku wejściowego, zawierającego skompresowany tekst (i być może dodatkowe informacje) oraz 
drzewa Huffmana przeprowadzić dekodowanie tekstu, zapisując odkodowany tekst do pliku 
wyjściowego.
*)


open Core.Std;;


(* type 'a htree = HNill | HLeaf of int * 'a | HC of int  * 'a htree * 'a htree ;; *)
type 'a htree = HNill | HLeaf of int * char | HC of int  * char htree * char htree ;; 
let t1 = HC (18, HC (8, HC (4, HC (2, HLeaf (1, 'e'), HLeaf (1, 'c')), HLeaf (2, 'a')), HLeaf (4, 'b')), HLeaf (10, 'd'))
;;

let occurences_list_to_htree_list l =
  let rec _transform l acc = 
    match l with
    | [] -> acc
    | hd::tl -> _transform tl @@ HLeaf (fst hd, snd hd )  :: acc 
  in _transform l []
;; 

let h_prob htree =
  match htree with
  | HC( x, _, _ ) -> x
  | HLeaf( x, _) -> x
  | HNill -> failwith "no probablity in HNill node"
;;

let htree_cmp t1 t2 =
  let p1 = h_prob t1
  and p2 = h_prob t2 in
  compare p1 p2
;;

let make_HTree l  =
  let rec _make_HTree l =
    match l with 
    | [] -> HNill
    | [hd] -> hd
    | _ ->
      let e1::e2::tl = List.sort ~cmp:htree_cmp l in
      _make_HTree @@ HC(h_prob e1  + h_prob e2 , e1, e2)::tl
  in _make_HTree  ( occurences_list_to_htree_list l )
;;

let codes htree =
  let rec _codes htree acc =
    match htree with
    | HNill -> []
    | HLeaf(_, c) -> [(c, List.rev acc)]
    | HC(_, h1, h2) -> _codes h1 (0::acc) @ _codes h2 (1::acc) 
  in 
  _codes htree []
;;

let count_occurences stream =
  let table = Hashtbl.Poly.create ()  in 
  Stream.iter (fun x -> 
      match Hashtbl.find table x with
      | None -> Hashtbl.add_exn table ~key:x ~data:1
      | Some y -> Hashtbl.replace table  ~key:x ~data:(y+1) 
    ) stream ;
  List.map (Hashtbl.to_alist table) ~f:(fun (k,d) -> (d, k) )
;;

let rec find_code letter codes =
  match codes with 
  | [] -> failwith "letter not found (find_code)"
  | (c, code)::tl -> if letter = c then code else find_code letter tl
;;

let split_by_8 l =
  let rec split_by_8 l n =
    if n < 8 
    then [l] 
    else [List.take l  8 ] @ split_by_8 (List.drop l 8) (n-8) 
  in split_by_8 l (List.length l)
;;

let  list_to_byte l =
  let rec process l = 
    match l with
    | [] -> 0
    | hd::tl -> hd + 2 * process tl (* uwaga: zamiana kolejnosci! *)
  in process l |> Char.of_int_exn 
;;

let byte_to_list b = 
  let rec process i n acc  =
    match n with 
    | 8 -> List.rev acc
    | _ -> process (i / 2 ) (n+1) (i mod 2 :: acc) in 
  process (Char.to_int b) 0 []
;; 

let rec make_encoded ll ~acc =
  match ll with 
  | [] -> (List.rev acc, []);
  | hd::tl ->
    if List.length hd = 8 
    then make_encoded tl ~acc:(list_to_byte hd :: acc)
    else (List.rev acc, hd)
;;

let rec repeat value times =
  match times with
  | 0 -> []
  | _ -> value :: repeat value (times-1)
;;

let fill_with_0 l =
  l @ repeat 0 (8 - List.length l)
;;


let encode stream =
  let cache = ref [] in
  let tmp = ref [] in 
  let rec next i = 
    match List.length !cache, Stream.peek stream  with 
    | 8, _ -> tmp := !cache ; cache := [] ; Some ( list_to_byte !tmp ) 
    | 0, None -> None
    | _, None -> tmp := !cache ; cache := [] ; Some ( list_to_byte ( fill_with_0 !tmp ))
    | _, Some m -> Stream.junk stream ; cache := !cache @ [m] ; next i 
  in Stream.from next
;;

let letters_strm_to_moves_strm htree stream =
  let cache = ref [] in 
  let letters_codes = codes htree in
  let rec next i = 
    match !cache with 
    | hd::tl -> cache := tl ; Some hd 
    | [] -> 
      match Stream.peek stream with 
      | None -> None
      | Some l -> 
        Stream.junk stream ;
        cache := find_code l letters_codes ;
        next i
  in Stream.from next
;;

let htree_of_file filename = 
  let in_channel = open_in filename in 
  try 
    let htree = 
      Stream.of_channel  in_channel |>
      count_occurences |>
      make_HTree  in
    In_channel.close in_channel ; 
    htree
  with e ->
    In_channel.close in_channel ;
    raise e
;;

let generate_out_file filename out_stream =
  let outc = Out_channel.create filename in 
  Stream.iter (fun x -> output_char outc x  ) out_stream ;
  Out_channel.close outc

;;


let process_in_file filename htree =
  let in_channel = open_in filename in 
  try  
    Stream.of_channel  in_channel |>
    letters_strm_to_moves_strm htree |>
    encode |>
    generate_out_file "out" ;
    In_channel.close in_channel   
  with e ->
    In_channel.close in_channel ;
    raise e
;;

let test_in_to_out () =
  htree_of_file "in" |>
  process_in_file "in" 
;;

let list_of_stream stream =
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result
;;

let decode htree stream =
  let node = ref htree in 
  let rec next i =
    match !node with 
    | HNill -> failwith "decode error"
    | HLeaf (_,c) -> node := htree ; Some c
    | HC (_, l, r) ->  
      match Stream.peek stream with
      | None -> None
      | Some x -> 
        Stream.junk stream ;
        if x = 0 then 
          node := l
        else node := r ; 
        next i 
  in Stream.from next
;;

let byte_strm_to_moves_strm stream =
  let cache = ref [] in 
  let rec next i = 
    match !cache with 
    | hd::tl -> cache := tl ; Some hd 
    | [] -> 
      match Stream.peek stream with 
      | None -> None
      | Some b -> 
        Stream.junk stream ;
        cache := byte_to_list b ;
        next i
  in Stream.from next
;;

let process_out_file fout htree =
  let in_channel = open_in fout in 
  try  
    Stream.of_channel  in_channel |>
    byte_strm_to_moves_strm |>
    decode htree |>
    generate_out_file "res" ;
    In_channel.close in_channel   
  with e ->
    In_channel.close in_channel ;
    raise e
;;

let test_conversion () =
  let htree = htree_of_file "in" in
  process_in_file "in" htree ;
  process_out_file "out" htree ;
;;

test_conversion () ;;

let test_case expression number =
  try 
    match expression with 
    | lazy (a, b) -> assert (a = b)
  with 
  | e ->  printf "test number %i failed" number ;
    raise e 
;;

let test () =
  let t1 = HC (18, HC (8, HC (4, HC (2, HLeaf (1, 'e'), HLeaf (1, 'c')), HLeaf (2, 'a')), HLeaf (4, 'b')), HLeaf (10, 'd'))
  and l1 = [(2,'a'); (4, 'b'); (1, 'c'); (10, 'd'); (1, 'e')] in
  try 
    test_case (lazy( make_HTree l1, t1)) 0; 
    test_case (lazy( codes t1, [('e', [0; 0; 0; 0]); ('c', [0; 0; 0; 1]); ('a', [0; 0; 1]); ('b', [0; 1]); ('d', [1])] )) 1;
    true
  with 
  | _ -> false
;;
(* Ala ma kota!
   Kot pije mleko.
   Pies goni kota. *)