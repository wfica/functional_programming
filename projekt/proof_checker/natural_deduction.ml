open Proof_type
open Core.Std
open Rules


module NatDed: 
sig
  val proof :
    ?known_truth: formula list ->
    Core.Std.Out_channel.t ->
    proof -> bool
end  
=
struct
  exception IncorrectProof of int


  let rec _proof (Proof(l)) known_truth boxes freshes =
    let is_item_OK i truth item boxes freshes =
      match item with
      | Formula(f) ->
        if Rules.inferable f truth boxes freshes 
        then ( f:: truth, boxes, freshes) 
        else raise (IncorrectProof(i))
      | Hypothesis(assumption, l2) ->
        match assumption with
        | Form(assum) ->
          if _proof l2 (assum::truth) boxes freshes
          then (truth, hypothesis_to_box item :: boxes , freshes)
          else raise (IncorrectProof(i))
        | Fresh(var) ->
          if _proof l2 truth boxes (var::freshes)
          then (truth, hypothesis_to_box item :: boxes , freshes)
          else raise (IncorrectProof(i))
        | Fresh_Form(var, form) ->
          if _proof l2 (form::truth) boxes (var::freshes)
          then (truth, hypothesis_to_box item :: boxes , freshes)
          else raise (IncorrectProof(i))
    in
    let _ = List.foldi l ~init:(known_truth,boxes,freshes)
        ~f:(fun i (truth, boxes, freshes) item -> is_item_OK i truth item boxes freshes)
    in true

  let proof ?(known_truth=[]) outx p  =
    try _proof p known_truth [] [] with
      IncorrectProof(msg) ->
      Out_channel.output_string outx @@ "  Błąd w dowodzie w linii numer " ^  string_of_int msg ^ "\n";
      false
end