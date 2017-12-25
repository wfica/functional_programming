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


(* funkcje do wypisywania formuł - pomocnicze, tylko do testowania *)
(* let rec print_formula  f = 
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
    output_task_list  tl *)


