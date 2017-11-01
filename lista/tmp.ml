
type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

                                (*    Up,        tree *)
type 'a place_btree = PlaceTree of 'a btree * 'a btree  ;;

let create_place_tree tree = 
		match tree with 
		| Leaf -> failwith "cannot point to Leaf (create)"
		| _ -> PlaceTree(Leaf, tree )
;;

let left (PlaceTree(u, Node(l, v, r))) = 
	match l with 
	| Leaf -> failwith "cannot point to no Leaf (left)"
	| _  -> PlaceTree( Node(r, v, u), l)
;;

let right (PlaceTree(u, Node(l, v, r))) =
	match r with 
	| Leaf -> failwith "cannot point po Leaf (right)"
	| _ -> PlaceTree( Node(u, v, l ), r )
;;

let up (PlaceTree(u, node)) =
	match u with
	| Leaf -> failwith "cannot point to Leaf (up)"
	| Node(ul, uv, ur) -> PlaceTree(ur, Node(ul, uv, node))
;;

let t1 = Node (Leaf, 5, Leaf);;
let t2 = Node ( Node(Leaf, 5, Leaf), 1, Node(Leaf, 7, Node(Leaf, 8, Leaf)));;
let t3 = Node (Node( Node(Leaf, 3, Leaf),2, Leaf), 1, Leaf) ;;
