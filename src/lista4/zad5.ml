(*
Zadanie 5 (6p.)

Przy modyfikacji struktur danych często chcemy wykonać więcej niż jedną operację w pobliskich 
miejscach. W przypadku czysto funkcyjnych struktur może to skutkować niepotrzebnie długim czasem 
wykonania, spowodowanym przechodzeniem i odtwarzaniem struktury danych przy każdej zmianie. Aby 
zapobiec temu problemowi, możemy rozbić operację modyfikacji struktury na dwie części: 
znajdowania miejsca w strukturze, w którym mamy wykonać zmianę, oraz przeprowadzenia zmiany w 
konkretnym miejscu; musimy też znaleźć odpowiednią pośrednią reprezentację dla takich miejsc.

1. Zdefiniuj typ danych α place jako pośrednią reprezentację dla typu α list.

2. Zaimplementuj funkcje findNth : α list -> int -> α place oraz collapse : α place -> 
α list. Pierwsza powinna lokalizować n-te miejsce na liście, druga — zapominać informację o 
miejscu i zwracać listę na której działamy.

3. Zaimplementuj funkcje add : α -> α place -> α place oraz del : α place -> α place, 
odpowiednio dodającą i usuwającą element w odpowiednim miejscu listy. Funkcje powinny działać 
w czasie stałym, oraz spełniać następujące równości: 
(a) collapse (add 3 (findNth [1;2;4] 2)) = [1;2;3;4]
(b) collapse (del (findNth [1;2;4] 2)) = [1;2]
(c) del (add x p) = p dla dowolnych x : α i p : α place

4. Zaimplementuj funkcje next : α place -> α place oraz prev : α place -> α place, 
służące do nawigacji w strukturze listy.

Listy nie są jedynym typem danych dla którego możemy definiować struktury pamiętające 
miejsce w którym pracujemy. Zdefiniuj typ danych α btplace będący pośrednią reprezentacją 
pozwalającą na modyfikacje drzew binarnych z Zadania 2. Napisz 3 funkcje up, left, right : α 
place -> α place do nawigacji po drzewie.
*)

type 'a place = Place of 'a list * 'a list;;

let findNth l n =
  let rec _findNth rev tail n =
    match n, tail  with 
    | 0, _ ->  Place(rev, tail)
		| _, h::t -> _findNth (h::rev) t (n-1)
		| _, [] -> failwith "n greater than list length"
  in 
  _findNth [] l (n+1)
;;


let collapse p =
	let rec _collapse rev acc =
	match rev with 
	| []-> acc
	| hd::tl -> _collapse tl (hd::acc)
	in match p with 
	| Place(rev, tl) -> _collapse rev tl
;; 


let add x (Place(ptr::rev, tail)) = Place(x::rev, ptr::tail);;
let del p =
	match p with
	| Place([_], []) -> failwith "cannot point po no place"
	| Place(_::tl, []) -> Place(tl, [])
	| Place(_::tl, h::t) -> Place(h::tl, t)
	| Place([], _) as p -> p (* UNREACHABLE! *)
;;

let next p = 
	match p with
	| Place(_, []) -> failwith "out of range"
	| Place(rev, hd::tl ) -> Place(hd::rev, tl)
;;

let prev p =
	match p with 
	| Place(hd::prev::tl, tail) -> Place(prev::tl, hd::tail)
	| Place([_], _) -> failwith "out of range"
	| Place([], _) as p -> p (* UNREACHABLE! *) 
;;

let test () =
	let p = findNth [1;2;3;4] 3 in 
	let q = findNth [1;2;3;4] 2 in
	let r = findNth [1;2;3;4] 0 in
	try
		assert( collapse p = [1;2;3;4] ) ;
		assert( collapse q = [1;2;3;4] ) ;
		assert( collapse r = [1;2;3;4] ) ;
		assert( collapse (add 3 (findNth [1;2;4] 2)) = [1;2;3;4] );
		assert( collapse (del (findNth [1;2;4] 2)) = [1;2] );
		assert( del (add 100000 p ) = p ) ;
		assert( del (add 100000 q ) = q ) ;
		assert( del (add 100000 r ) = r ) ;
		assert( prev p = q) ;
		assert( next q = p) ;
		assert( prev @@ prev @@ prev p = r) ;
		assert( next @@ next @@ next r = p) ;
		true
	with
	 _ -> false
;;

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

                                (*    Up,        tree *)
type 'a place_btree = PlaceTree of 'a btree * 'a btree * bool list ;;

let create_place_tree tree = 
		match tree with 
		| Leaf -> failwith "cannot point to Leaf (create)"
		| _ -> PlaceTree(Leaf, tree ,[])
;;

let tree_of_place (PlaceTree(Leaf, node, [])) = node;;

let left (PlaceTree(u, Node(l, v, r), lb)) = 
	match l with 
	| Leaf -> failwith "cannot point to no Leaf (left)"
	| _  -> PlaceTree( Node(u, v, r), l, false::lb)
;;

let right (PlaceTree(u, Node(l, v, r), lb)) =
	match r with 
	| Leaf -> failwith "cannot point po Leaf (right)"
	| _ -> PlaceTree( Node(l, v, u ), r , true::lb)
;;

let up (PlaceTree(u, node, lb)) =
	match u, lb with
	| Leaf, _ | _, [] -> failwith "cannot point to Leaf (up)"
	| Node(ul, uv, ur), false::tail -> PlaceTree(ul, Node(node, uv, ur), tail)
	| Node(ul, uv, ur), true::tail -> PlaceTree(ur, Node(ul, uv, node), tail)
;;

let t1 = Node (Leaf, 5, Leaf);;
let t2 = Node ( Node(Leaf, 5, Leaf), 1, Node(Leaf, 7, Node(Leaf, 8, Leaf)));;
let t3 = Node (Node( Node(Leaf, 3, Leaf),2, Leaf), 1, Leaf) ;;
let t4 = Node( t2, 10, t3);;

let test_tree () = 
	try 
		assert ( tree_of_place @@  up @@ left @@ create_place_tree t3  = t3 );
		assert ( tree_of_place @@  up @@ up @@ left @@ left @@ create_place_tree t4  = t4 );
		assert ( tree_of_place @@  up @@ up @@ right @@ left @@ create_place_tree t4  = t4 );
		assert (up @@ right @@ left @@ create_place_tree t4  = up @@ left @@ right @@ create_place_tree t4  );
		true
	with
	_ -> false
;;


