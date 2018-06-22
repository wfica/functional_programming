(*
Zadanie 3 (6p.)

Zdefiniuj typ służący do reprezentacji formuł rachunku zdań składających 
się ze zmiennych zdaniowych, negacji, koniunkcji i alternatywy.

(a) Napisz funkcję sprawdzającą, czy dana formuła jest tautologią. W tym celu 
należy generować kolejne wartościowania zmiennych zdaniowych występujących 
w danej formule i sprawdzać dla nich wartość formuły. W przypadku, gdy 
formuła nie jest tautologią, funkcja powinna, oprócz tej informacji, podać 
jedno z wartościowań, dla których formuła nie jest prawdziwa.

(b) Napisz funkcję, która przekształca zadaną formułę w formułę jej 
równoważną w negacyjnej postaci normalnej, tj. w której negacja występuje 
tylko przy zmiennych zdaniowych.

(c) Napisz funkcję, która przekształca zadaną formułę w formułę jej 
równoważną w koniunkcyjnej postaci normalnej.

(d) Napisz funkcję sprawdzającą (syntaktycznie), czy zadana formuła jest 
tautologią, korzystając z faktu, że każdą formułę można przedstawić w 
koniunkcyjnej postaci normalnej.

(e) Napisz funkcję, która przekształca zadaną formułę w formułę jej 
równoważną w dyzjunkcyjnej postaci normalnej.

(f) Napisz funkcję sprawdzającą (syntaktycznie), czy zadana formuła jest 
sprzeczna, korzystając z faktu, że każdą formułę można przedstawić w 
dyzjunkcyjnej postaci normalnej.
*)
;;

open Core.Std;;

type 'a formula = 
  | Var of 'a
  | Not of 'a formula
  | And of 'a formula list
  | Or of 'a formula list
;;


let x = Not( Or([  Var('p'); Var('q') ]) ) ;; (*  ~(p v q)  *)
let y = Or([Var('a'); And([Var('b'); Var('c')]) ]);; (* a v b i c *)
let z = Not(Or( [And( [Var('a'); Var('b')]); Not( And([Var('c');Var('d')])) ]) );; (* ~( a i b v ~(c i d) ) *)
let t = Not(And([Not(y); x; z]));; (* ~( x i ~y i z ) *)
let excluded_middle = Or([Var('p'); Not( Var('p') ) ]);; (* p v ~p *)
let excluded_middle2 = Or([t; Not(t)]);;


let variables formula =
  let rec _vars  acc formula =
    match formula with
    | Var(var) -> var::acc
    | Not(f) -> _vars acc f
    | And(l) | Or(l) -> List.fold  l ~init:acc ~f:_vars 
  in List.dedup @@ _vars [] formula
;;

let zeros n =
  let rec _zeros n acc = 
    match n with
    | 0 -> acc
    | _ -> _zeros (n-1) (0::acc)
  in
  _zeros n []
;;

let next_binary_seq seq n =
  match List.findi (List.rev seq) ~f:(fun _ elem -> elem = 0) with
  | None -> zeros n
  | Some(j, _) -> List.take seq (n-1-j) @ [1] @ zeros j
;;

let all_binary_seq n =
  let z = zeros n in
  let rec _all_binary_seq acc last n=
    let next = next_binary_seq last n in  
    if List.exists last ~f:(fun x -> x = 0) then _all_binary_seq (next::acc) next n 
    else acc
  in _all_binary_seq [z] z n
;;  

let rec evaluate formula env =
  match formula with 
  | Var(var) -> List.find_exn env ~f:(fun (variable, _) -> variable = var ) |> snd 
  | Not(f) -> 1 - evaluate f env
  | Or(l) ->  if List.find l ~f:(fun formula -> evaluate formula env = 1)  = None then 0 else 1
  | And(l) -> List.fold l ~init:1 ~f:(fun acc form -> acc * evaluate form env )
;;


let equal f1 f2 =
  let vars1 = List.sort ~cmp:compare (variables f1) 
  and vars2 = List.sort ~cmp:compare (variables f2) in
  if vars1 <> vars2 then false else 
    all_binary_seq (List.length vars1) |> 
    List.map ~f:(List.zip_exn vars1) |>
    List.for_all ~f:(fun env -> evaluate f1 env = evaluate f2 env)
;;


let brute_is_tautology formula =
  let vars = variables formula in
  all_binary_seq (List.length vars) |>
  List.map ~f:(List.zip_exn vars) |>
  List.find ~f:(fun env -> evaluate formula env = 0 )   
;;

let is_tautology formula =
  match brute_is_tautology formula   with
  | None -> Ok(true)
  | Some(x) -> Error(x)
;;


let rec nnf formula = 
  match formula with 
  | Not( Or l) -> And(List.map l ~f:(fun x -> nnf (Not x)) ) 
  | Not( And l) -> Or(List.map l ~f:(fun x -> nnf(Not x)) )
  | Not( Not f) -> nnf f
  | Not( Var _) as f-> f
  | And l -> And(List.map l ~f: nnf)
  | Or l -> Or(List.map l ~f: nnf) 
  | f -> f 
;;


let get_ands l =
  let rec _get_ands l vars ors flat_ands =
    match l with
    | [] -> (vars, ors, flat_ands)
    | And(hd)::tl -> _get_ands tl vars ors  (And(hd)::flat_ands)
    | Or(hd)::tl -> _get_ands tl vars (hd @ ors) flat_ands
    | Var(hd)::tl -> _get_ands tl (Var(hd)::vars) ors flat_ands
    | Not(hd)::tl -> _get_ands tl (Not(hd)::vars) ors flat_ands
  in _get_ands l [] [] []
;;

let get_ors l =
  let rec _get_ors l vars flat_ors ands =
    match l with
    | [] -> (vars, flat_ors, ands)
    | And(hd)::tl -> _get_ors tl vars flat_ors  ( hd @ ands)
    | Or(hd)::tl -> _get_ors tl vars (Or(hd):: flat_ors) ands
    | Var(hd)::tl -> _get_ors tl (Var(hd)::vars) flat_ors ands
    | Not(hd)::tl -> _get_ors tl (Not(hd)::vars) flat_ors ands
  in _get_ors l [] [] []
;;

let flatten formula =
  let rec _flatten nnf_formula =
    match nnf_formula with 
    | Not _ as f -> f
    | Var _ as f -> f
    | And(l) -> 
      let flat =  List.map l ~f:_flatten in
      let (vars, flat_ors, ands) = get_ors flat in 
      And( flat_ors @ vars @ ands )
    | Or(l) ->   
      let flat =  List.map l ~f:_flatten in
      let (vars, ors, flat_ands) = get_ands flat in 
      Or( flat_ands @ vars @ ors )
  in _flatten @@ nnf formula
;;

let get_and l =
  let rec _get_and l acc acc_ands = 
    match l with
    | [] -> (acc, acc_ands)
    | hd::tl -> match hd with
      | And _ as f -> _get_and tl acc (f::acc_ands)
      | _ as f ->  _get_and tl (f::acc) acc_ands
  in let (rest, ands) =  _get_and l [] []  in
  match ands with 
  | [] -> (rest, None)
  | hd::tl -> (tl @ rest , Some hd)
;;


let cnf formula =
  let rec _nnf2cnf formula = 
    match formula with
    | Var _ as f -> f
    | Not _ as f -> f
    | And(l) -> And( List.map l ~f:_nnf2cnf )
    | Or(l)-> 
      let (tl, andd) = get_and (List.map l ~f:_nnf2cnf) in
      match andd with  
      | None -> Or(tl)
      | Some And ([x; y]) -> And([_nnf2cnf (Or(x::tl)); _nnf2cnf (Or(y::tl)) ])
      | Some And( x::rest ) -> And([_nnf2cnf (Or(x::tl)); _nnf2cnf (Or( And(rest)  :: tl)) ])
      | _ -> formula (* UNREACHABLE! *)
  in  flatten @@  _nnf2cnf @@ flatten @@ nnf formula
;;


let var_neg var =
  match var with
  | Var(x) as f-> Not(f)
  | Not(y) -> y
;;


let is_tautology_cnf formula =
  let always_true form = 
    match form with
    | Not _ -> false
    | Var _ -> false
    | Or(l) -> List.exists l ~f:(fun x -> List.exists l ~f:(fun y -> var_neg x = y))
    | _ -> false (* UNREACHABLE *)
  in
  match cnf formula with 
  | And(l) -> List.for_all l ~f:(fun x -> always_true x )
  | _ as f ->  always_true f
;;



let get_or l =
  let rec _get_or l acc acc_ors = 
    match l with
    | [] -> (acc, acc_ors)
    | hd::tl -> match hd with
      | Or _ as f -> _get_or tl acc (f::acc_ors)
      | _ as f ->  _get_or tl (f::acc) acc_ors
  in let (rest, ors) =  _get_or l [] []  in
  match ors with 
  | [] -> (rest, None)
  | hd::tl -> (tl @ rest , Some hd)
;;


let dnf formula =
  let rec _nnf2dnf formula = 
    match formula with
    | Var _ as f -> f
    | Not _ as f -> f
    | Or(l) -> Or( List.map l ~f:_nnf2dnf )
    | And(l)-> 
      let (tl, orr) = get_or (List.map l ~f:_nnf2dnf) in
      match orr with  
      | None -> And(tl)
      | Some Or ([x; y]) -> Or([_nnf2dnf (And(x::tl)); _nnf2dnf (And(y::tl)) ])
      | Some Or( x::rest ) -> Or([_nnf2dnf (And(x::tl)); _nnf2dnf (And( Or(rest)  :: tl)) ])
      | _ -> formula (* UNREACHABLE! *)
  in  flatten @@  _nnf2dnf @@ flatten @@ nnf formula
;;


let is_satisfiable formula =
  let can_be_true form = 
    match form with 
    | Var _ -> true
    | Not _ -> true
    | And(l) -> List.for_all l ~f:(fun x -> List.for_all l ~f:(fun y -> var_neg x <> y ))
    | _ -> false (* UNREACHABLE! *)
  in match dnf formula with
  | Or(l) -> List.exists l ~f:(fun x -> can_be_true x)
  | _ as f -> can_be_true f
;; 


let test_function f l =
  assert ( List.for_all l ~f:(fun x -> equal x (f x) ) )
;;

let test_functions f1 f2 l = 
  assert( List.for_all  l ~f:(fun x -> f1 x = f2 x))
;;

let test () =
  let x = Not( Or([  Var('p'); Var('q') ]) ) in (*  ~(p v q)  *)
  let y = Or([Var('a'); And([Var('b'); Var('c')]) ]) in (* a v b i c *)
  let z = Not(Or( [And( [Var('a'); Var('b')]); Not( And([Var('c');Var('d')])) ]) ) in  (* ~( a i b v ~(c i d) ) *)
  let t = Not(And([Not(y); x; z])) in (* ~( x i ~y i z ) *)
  let u = Var('u') in 
  let v = Not(u) in 
  let w = Or([u; u]) in 
  let q = And([w; Not(w)]) in
  let excluded_middle = Or([Var('p'); Not( Var('p') ) ]) in  (* p v ~p *)
  let excluded_middle2 = Or([t; Not(t)]) in
  let examples = [x; y; z; t; u; v; w; q; excluded_middle; excluded_middle2] in
  try
    test_function cnf examples ;  
    test_functions is_tautology_cnf (fun x -> if brute_is_tautology x = None then true else false ) examples;
    test_function dnf examples ;  
    test_functions (fun x -> not (is_satisfiable x ) )  (fun x -> if brute_is_tautology (Not(x)) = None then true else false ) examples;
    true
  with 
  | _ -> false
;;