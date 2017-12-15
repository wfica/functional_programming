open Proof_type
open Core.Std 
open Rules 

module NatDed :
sig 
  val proof : 
    ?known_truth:Proof_type.formula list ->
    Core.Std.Out_channel.t ->
    Proof_type.proof -> bool
end = 
struct
  exception IncorrectProof of int
  let hypothesis_goal h =
    match h with 
    | Hypothesis(_, Proof(proof)) ->  List.last_exn proof |> item_to_formula 
    | _ -> failwith "cannot get goal of a Formula"


  let rec _proof (Proof(l)) known_truth =
    let is_item_OK i truth item =
      match item with 
      | Formula(f) -> 
        if Rules.inferable f truth then f else raise (IncorrectProof(i))
      | Hypothesis(assumption, l2) as x-> 
        if _proof l2 (assumption::truth)
        then Impl(assumption, hypothesis_goal x) 
        else raise (IncorrectProof(i))
    in 
    let _ = List.foldi l ~init:known_truth ~f:(fun i truth item -> (is_item_OK i truth item)  :: truth )
    in true

  let proof ?(known_truth=[]) outx p  =
    try _proof p known_truth with 
      IncorrectProof(msg) -> 
      Out_channel.output_string outx @@ "  Błąd w dowodzie w linijce numer " ^  string_of_int msg ^ "\n"; 
      false 
end