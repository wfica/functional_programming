open Proof_type
open Core.Std


module Rules :  
sig
  val inferable: Proof_type.formula -> Proof_type.formula list -> bool 
end =
struct 
  let contains l elem = List.exists l ~f:(fun x -> x = elem) 
  let introduction goal truth = 
    let is_true = contains truth in 
    match goal with 
    | And(x, y) -> is_true x && is_true y
    | Or(x, y) -> is_true x || is_true y 
    | Impl(_, _ ) as f -> is_true f
    | Const(x) -> x 
    | Not(x) -> is_true (Impl(x, Const(false)))
    | Variable(_) as v -> is_true v 
    | Iff(x, y) -> is_true (Impl(x, y)) &&  is_true (Impl(y, x))

  let elimination goal truth =
    let is_true = contains truth in 
    let eliminate formula goal = 
      match formula with 
      | And(x, y) -> x = goal || y = goal
      | Or(x, y) -> is_true (Impl(x, goal)) && is_true (Impl(y, goal))
      | Impl(x, y) -> y =  goal && is_true x 
      | Const(x) -> x = false
      | Variable(_) -> true
      | Iff(x, y) -> x = goal && is_true y || y = goal && is_true x  
      | Not(x)  -> 
        if goal = Const(false) then is_true x 
        else match x with 
          | Not(y) -> y = goal 
          | _ -> false
    in 
    List.exists truth ~f:(fun f -> eliminate f goal)

  let inferable goal truth = 
    introduction goal truth || elimination goal truth 
end 