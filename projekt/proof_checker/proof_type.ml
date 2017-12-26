open Core.Std

type term = Var of string | Fun of string * term list

type formula = 
  | Pred of string * term list 
  | Const of bool
  | Not of formula
  | And of formula * formula
  | Or of formula * formula 
  | Impl of formula * formula 
  | Iff of formula * formula
  | All of string * formula
  | Exists of string * formula

type assumption = Form of formula | Fresh of string | Fresh_Form of string * formula
type box = assumption * formula

type proof_item = Formula of formula | Hypothesis of assumption * proof
and  proof = Proof of proof_item list

type task = Task of string * formula * proof

let item_to_formula item = 
  match item with 
  | (Formula(f)) -> f 
  | _ -> failwith "cannot convert hypothesis to formula"

let get_double_negated formula = 
  match formula with 
  | Not(Not(x)) -> Some x
  | _ -> None

let is_task_valid (Task(_, goal, Proof(proof))) =
  match List.last proof with
  | None -> false
  | Some item -> item_to_formula item = goal 

let rec fv_of_term term =
  match term with
  | Var(x) -> [x]
  | Fun(_, l) -> List.concat_map  l ~f:(fun t -> fv_of_term t)

let rec list_diff a b =
  match a with
  | [] -> []
  | hd::tl -> 
    let rest = list_diff tl b in 
    if List.mem b hd then rest else hd::rest

let rec fv_of_formula formula =
  match formula with
  | Pred(_, l) -> List.concat_map  l ~f:(fun t -> fv_of_term t) 
  | Const(_) -> []
  | Not(f) -> fv_of_formula f
  | And(a, b) | Or(a, b) | Iff(a, b) | Impl(a, b) -> fv_of_formula a @ fv_of_formula b 
  | All(x, f) | Exists(x, f)-> fv_of_formula f |> list_diff [x]


let hypothesis_goal h =
  match h with 
  | Hypothesis(_, Proof(proof)) ->  List.last_exn proof |> item_to_formula 
  | _ -> failwith "cannot get a goal of a Formula"

let hypothesis_to_box h  : box= 
  match h with
  | Hypothesis(f, _) ->  f, (hypothesis_goal h)
  | _ -> failwith " cannot get a box out of a Formula"

let rec terms_of_term t = 
  match t with
  | Var(_) -> [t]
  | Fun(_, l) -> t::List.concat_map l ~f:terms_of_term 

let rec terms_of_formula f =
  match f with
  | Pred(_, l) -> List.concat_map l ~f:terms_of_term 
  | Const(_) -> []
  | Not(x) -> terms_of_formula x
  | And(a, b)| Or(a, b) | Impl(a, b) | Iff(a,b) -> terms_of_formula a @ terms_of_formula b
  | Exists(_, x) | All(_, x) -> terms_of_formula x

let terms_of_axioms (a : formula list)  =
  List.concat_map a ~f:terms_of_formula |>
  List.dedup 

let terms_of_assumption (a : assumption)=
  match a with
  | Form(f) -> terms_of_formula f
  | Fresh(v) -> [Var(v)]
  | Fresh_Form(v, f) -> (Var v) :: terms_of_formula f 

let rec terms_of_proof_item (p : proof_item) = 
  match p with
  | Formula(f) -> terms_of_formula f
  | Hypothesis(assumption, proof) -> terms_of_assumption assumption @ terms_of_proof proof
and terms_of_proof (Proof(l)) = 
  List.concat_map l ~f:terms_of_proof_item |>
  List.dedup

let terms_of_task (Task(_, goal, proof)) =
  terms_of_formula goal @ terms_of_proof proof |>
  List.dedup

(* substitutes subs for x in term *)
let rec substitute_term ~subs x term  = 
  match term with 
  | Var(t) -> if t = x then subs else term
  | Fun(f, l) -> Fun( f, List.map l ~f:(substitute_term x ~subs)) 

(* let  subs_vaild ~subs x f bound_vars : bool =  
  true  *)

(* substitutes subs for x in formula f *)
let substitute ~subs x f  = 
  let rec _substitute ~subs x f  = 
    let sub = _substitute ~subs x in 
    match f with 
    | Pred(p, l) -> Pred(p, List.map l ~f:(substitute_term ~subs x ) )
    | Const(_) -> f
    | Not(a) -> Not(sub a)
    | And(a, b) -> And( sub a, sub b )
    | Or( a, b) -> Or( sub a, sub b)
    | Iff(a, b) -> Iff(sub a, sub b)
    | Impl(a, b) -> Impl(sub a, sub b)
    | All(y, f) -> All(y, sub f)
    | Exists(y, f) -> All(y, sub f)
  in 
  (* if subs_vaild ~subs x f [] 
  then _substitute ~subs x f 
  else failwith "invalid substitute" *)
  _substitute ~subs x f 


(* funkcje do wypisywania formuł - pomocnicze, tylko do testowania *)
let rec print_term t =
  match t with
  | Var(x) -> printf "%s" x 
  | Fun(f, l) -> printf "%s(" f ; List.iter l ~f:print_term ; printf ")"


let rec print_formula  f = 
  match f with 
  | Pred(p, l) -> printf "%s(" p; List.iter l ~f:print_term ; printf ")" 
  | Const(true) -> printf "T" 
  | Const(false) -> printf "F" 
  | Not(f) -> printf "~(" ;  print_formula  f ; printf ")"
  | And(f1, f2) -> printf "AND("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | Or(f1, f2) -> printf "Or("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | Impl(f1, f2) -> printf "Impl("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | Iff(f1, f2) -> printf "Iff("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | All(x, f) -> printf "A %s " x ; print_formula f 
  | Exists(x, f) -> printf "E %s " x ; print_formula f 

let print_assumption a =
  match a with 
  | Form(f) -> print_formula f
  | Fresh(var) -> printf "fresh %s " var
  | Fresh_Form (var, f) -> printf "fresh %s " var ; print_formula f 

let rec print_proof (Proof(proof)) =
  let  print_proof_item item =
    match item with 
    | Formula(f) -> print_formula f 
    | Hypothesis(a, p) ->  printf "[ "; print_assumption a ; printf " : "; print_proof p ; printf " ]" 
  in 
  List.iter proof ~f:(fun item -> print_proof_item item; printf ";\n" )

let rec output_task_list  task_list = 
  match task_list with 
  | [] -> () 
  | (Task(id, target, proof) )::tl ->   
    printf "%s\n" id ;
    print_formula  target ;
    printf "\n" ; 
    print_proof  proof ; printf "------------\n";
    output_task_list  tl


