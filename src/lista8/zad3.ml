(*
Zadanie 3 (12p.)

Chcemy stworzyć sygnaturę dla funkcyjnych reprezentacji grafów skierowanych, sparametryzowanych 
przez abstrakcyjne typy wierzchołków i krawędzi. W tym celu tworzymy sygnaturę dla typu 
wierzchołków:

module type VERTEX =
sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label  
end                 
gdzie typ t reprezentuje abstrakcyjny typ wierzchołków, typ label reprezentuje abstrakcyjny typ 
etykiet wierzchołków, a funkcja equal pozwala porównywać wierzchołki. Funkcja create tworzy 
nowy wierzchołek na podstawie etykiety, a funkcja label zwraca etykietę wierzchołka.

Napisz analogiczną sygnaturę dla typu krawędzi przyjmując, że krawędzie również mogą być 
etykietowane, porównywane, oraz dla każdej krawędzi powinna istnieć możliwość wyznaczenia 
jej wierzchołka początkowego i końcowego.
Zaimplementuj moduł Vertex spełniający sygnaturę VERTEX, a także moduł Edge spełniający 
sygnaturę EDGE.
Rozważmy następnie sygnaturę dla grafów skierowanych:

module type GRAPH =
sig
  (* typ reprezentacji grafu *)
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex

  type edge = E.t

  (* funkcje wyszukiwania *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  (* funkcje modyfikacji *) 
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t

  (* iteratory *)
  val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
end
Wykorzystując moduły Vertex i Edge z punktu 2., zaimplementuj moduł Graph zgodny z sygnaturą 
GRAPH dla dowolnie wybranej reprezentacji funkcyjnej grafu. (Funkcje succ i pred wyznaczają 
odpowiednio listę następników i poprzedników danego wierzchołka, a funkcje succ_e i pred_e 
wyznaczają odpowiednio listę krawędzi wychodzących i wchodzących do danego wierzchołka.)
Przetestuj działanie swojej implementacji na przykładowych danych.
Napisz funktor, który przyjmując jako argumenty moduły V:VERTEX oraz E:EDGE zwraca moduł zgodny 
z sygnaturą GRAPH.
Korzystając z sygnatury GRAPH napisz funkcje przechodzenia grafu w głąb i wszerz. Przetestuj te 
funkcje na swojej implementacji.
*)
open Core.Std

module type VERTEX =
sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label  
end  

module type EDGE =
sig
  type t
  type vertex
  type label

  val equal : t -> t -> bool
  val create :  vertex -> vertex -> label -> t
  val label : t -> label
  val succ : t -> vertex 
  val pred : t -> vertex
end



let create_vertex_module (type a) compare =
  (module struct
    type t = a
    type label = a
    let equal x y = compare x y = 0
    let create lab = lab
    let label ver = ver
  end : VERTEX with type t = a and type label = a)

(* jak zrobić create_vertex_module bez funkcji compare - funkcji która precyzuje typ ?? *) (* <--------------------------- *)


let create_edge_module (type a) (type b) eq_fun =
  (module struct
    type vertex = a
    type label = b
    type t = vertex * vertex * label
    let equal x y = eq_fun x y
    let create u v lab = (u, v, lab)
    let label (_, _, lab) = lab
    let succ (_,v,_) = v
    let pred (u, _, _) = u 
  end : EDGE with type vertex = a and  type label = b and type t = a * a * b )



module type GRAPH =
sig
  (* typ reprezentacji grafu *)
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex

  type edge = E.t

  (* funkcje wyszukiwania *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge option
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  (* funkcje modyfikacji *) 
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t

  (* iteratory *)
  val fold_v : ('a -> vertex -> 'a) -> t -> 'a -> 'a
  val fold_e : ('a -> edge  -> 'a) -> t -> 'a -> 'a
  val map_e : (edge -> 'a) -> t -> 'a list
  val iter_e : (edge -> unit ) -> t -> unit
  val map_v : (vertex -> 'a) -> t -> 'a list
  val iter_v : (vertex -> unit ) -> t -> unit

  (* trawersy *)
  val dfs : t -> vertex -> V.label list
  val bfs : t -> vertex -> V.label list
end


module Graph (V : VERTEX) (E : EDGE with type vertex = V.t )  : GRAPH with  module V = V and module E = E =
struct
  type t =  (V.t list) * (E.t list)
  module V = V
  type vertex = V.t
  module E = E 
  type edge = E.t
  let mem_v g v = List.exists (fst g) ~f:(fun u -> u = v) 
  let mem_e g e = List.exists (snd g) ~f:(fun e2 -> e = e2) 
  let mem_e_v g u v = List.exists (snd g) ~f:(fun e -> E.pred e = u && E.succ e = v)
  let find_e g u v = List.find (snd g ) ~f:(fun e -> E.pred e = u && E.succ e = v)
  let succ_e g v =
    if mem_v g v = false then [] 
    else List.fold (snd g) ~init:[] ~f:(fun acc e -> if E.pred e = v then e:: acc else acc)
  let pred_e g v =
    if mem_v g v = false then [] 
    else List.fold (snd g) ~init:[] ~f:(fun acc e -> if E.succ e = v then e:: acc else acc)
  let succ g v = List.map (succ_e g v) ~f:(fun e -> E.succ e)
  let pred g v = List.map (pred_e g v) ~f:(fun e -> E.pred e)

  let empty = ([],[])
  let add_e g e = 
    let (vs, es) = g in 
    if mem_e g e then g else
      let u = E.pred e and v = E.succ e in 
      match mem_v g u, mem_v g v with
      | false, false -> if V.equal u v  then (u::vs, e::es) else (u::v::vs, e::es)
      | true, true   -> (vs, e::es)
      | false, true  -> (u::vs, e::es)
      | true, false  -> (v::vs, e::es)
  let add_v g v = if mem_v g v then g else (v::fst g, snd g)
  let generate_vs es =
    let add vs v =
      match List.find vs ~f:(fun u -> V.equal u v) with 
      | None -> v::vs 
      | Some _ -> vs in 
    let rec aux es acc =
      match es with 
      | [] -> acc 
      | e::tl -> aux tl  (add (add acc (E.pred e )  ) (E.succ e) )
    in aux es []
  let rem_e g e =
    if mem_e g e = false then g else
      let new_es = List.filter (snd g) ~f:(fun edge -> E.equal e edge = false) in 
      (generate_vs new_es, new_es)
  let rem_v (vs, es) v =
    let new_vs = List.filter vs ~f:(fun u -> V.equal u v = false) in 
    let new_es = List.filter es ~f:(fun e -> V.equal (E.pred e) v = false && V.equal (E.succ e) v = false)
    in (new_vs, new_es)

  let fold_v f g init = List.fold (fst g) ~f ~init
  let fold_e f g init = List.fold (snd g) ~f ~init
  let map_e f g = List.map (snd g) ~f 
  let iter_e f g = List.iter (snd g) ~f
  let map_v f g = List.map (fst g) ~f
  let iter_v f g = List.iter (fst g) ~f

  let dfs g v =
    let vst = Hash_set.Poly.create () 
    and stack = Stack.create () 
    and result = ref [] in 
    Stack.push stack v ; 
    Stack.until_empty stack (fun u -> 
        if Hash_set.mem vst u then () 
        else 
          (result := V.label u :: !result ;
           Hash_set.add vst u ;
           succ_e g u |>
           List.iter ~f:(fun e -> Stack.push stack (E.succ e) ) )
      ) ;
    !result 

  let bfs g v =
    let vst = Hash_set.Poly.create () 
    and queue = Queue.create () 
    and result = ref [] in 
    Queue.enqueue queue v ; 
    while Queue.is_empty queue = false do
      let u = Queue.dequeue_exn queue in 
      if Hash_set.mem vst u then () 
      else 
        (result := V.label u :: !result ;
         Hash_set.add vst u ;
         succ_e g u |> 
         List.iter ~f:(fun e -> Queue.enqueue queue (E.succ e) ) )
    done ;
    !result 
end

let create_graph_module (type ver) (type edge_lab) (ver_eq : ver -> ver -> int)  (egde_eq : ver * ver * edge_lab ->  ver * ver * edge_lab -> bool) =
  let ver_mod = create_vertex_module ver_eq 
  and edge_mod = create_edge_module egde_eq in 
  (* let module MM = (Graph (val ver_mod) (val edge_mod)) in  *)
  let module V = (val ver_mod) in 
  let module E = (val edge_mod) in 
  let module G = Graph (V) (E) in 
  (module Graph (V) (E) : GRAPH)


module V = (val create_vertex_module Int.compare)
module E = (val (create_edge_module  (fun ((u : int), (v:int), (lab:string)) (x, y, lab2)-> u = x && v = y && lab = lab2) ))
module G = Graph (V) (E) 

(* module GG = (val (create_graph_module Int.compare (fun ((u : int), (v:int), (lab:string)) (x, y, lab2)-> u = x && v = y && lab = lab2) )) *)

let test_modulo  = 10
let test_n = 1000 
let rec generate_edges n acc=
  match n with
  | 0 -> acc
  | _ -> 
    let x = Random.int test_modulo and y = Random.int test_modulo in let label = string_of_int x ^ " --> " ^ string_of_int y in 
    generate_edges (n-1) ((E.create (V.create x ) (V.create  y ) label) :: acc)

let rec generate_vertices n acc =
  match n with
  | 0 -> acc
  | _ -> 
    let x = Random.int test_modulo in 
    generate_vertices (n-1) ((V.create x)::acc)  

let print_e e =  print_string (E.label e) ; print_newline ()
let print_es g =  G.iter_e (fun e ->  print_e e ) g 

let print_v v = print_int (V.label v) ; print_newline () 
let print_vs g = G.iter_v (fun v -> print_v v) g

let test_graph  =
  let vs = Array.create ~len:10 (V.create 0) in 
  Array.iteri ~f:(fun i  _ -> Array.set vs i (V.create i) ) vs ;
  let es = [
    E.create (Array.get vs 0 ) (Array.get vs 1 ) "0-->1";
    E.create (Array.get vs 1 ) (Array.get vs 2 ) "1-->2";
    E.create (Array.get vs  0) (Array.get vs 3 ) "0-->3";
    E.create (Array.get vs  3) (Array.get vs 4 ) "3-->4";
    E.create (Array.get vs  3) (Array.get vs 4 ) "3-->4";
    E.create (Array.get vs  4) (Array.get vs 2 ) "4-->2";
    E.create (Array.get vs  5) (Array.get vs 6 ) "5-->6";
    E.create (Array.get vs  2) (Array.get vs 1 ) "2-->1";
    E.create (Array.get vs  2) (Array.get vs 3 ) "2-->3";
  ] in 
  let graph = List.fold es ~f:(fun  acc e -> G.add_e acc e  ) ~init:G.empty in 
  print_string "DFS:\n" ;
  G.dfs graph (Array.get vs 0) |>
  List.iter ~f:(fun v -> print_v v ) ;
  print_string "BFS:\n" ;
  G.bfs graph (Array.get vs 0) |>
  List.iter  ~f:(fun v -> print_v v ) ;



  (* make clean && make zad3.native && ./zad3.native && make clean *)