type formula = 
  | Variable of string
  | Const of bool
  | Not of formula
  | And of formula * formula
  | Or of formula * formula 
  | Impl of formula * formula 
  | Iff of formula * formula

type proof_item = Formula of formula | Hypothesis of formula * proof
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

open Core.Std
let is_task_valid (Task(_, goal, Proof(proof))) =
  match List.last proof with
  | None -> false
  | Some item -> item_to_formula item = goal 

(* funkcje do wypisywania formuł - pomocnicze, tylko do testowania *)
let rec print_formula  f = 
  match f with 
  | Variable(s) -> printf "%s" s 
  | Const(true) -> printf "T" 
  | Const(false) -> printf "F" 
  | Not(f) -> printf "~(" ;  print_formula  f ; printf ")"
  | And(f1, f2) -> printf "AND("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | Or(f1, f2) -> printf "Or("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | Impl(f1, f2) -> printf "Impl("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "
  | Iff(f1, f2) -> printf "Iff("; print_formula  f1 ; printf ", " ; print_formula  f2 ; printf ") "

let rec print_proof (Proof(proof)) =
  let  print_proof_item item =
    match item with 
    | Formula(f) -> print_formula f 
    | Hypothesis(f, p) ->  printf "[ "; print_formula f ; printf " : "; print_proof p ; printf " ]" 
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