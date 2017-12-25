open Proof_type
open Core.Std
open Rules


module NatDed: 
sig
  val proof :
    ?known_truth: formula list ->
    Core.Std.Out_channel.t ->
    proof ->
    term list ->
    bool
end  
=
struct
  exception IncorrectProof of int


  let rec _proof (Proof(l)) known_truth boxes terms =
    let is_item_OK i truth item boxes terms =
      match item with
      | Formula(f) ->
        if Rules.inferable f truth boxes  terms
        then ( f:: truth, boxes) 
        else raise (IncorrectProof(i))
      | Hypothesis(assumption, l2) ->
        match assumption with
        | Form(assum) ->
          if _proof l2 (assum::truth) boxes terms
          then (truth, hypothesis_to_box item :: boxes )
          else raise (IncorrectProof(i))
        | Fresh(_) ->
          if _proof l2 truth boxes terms
          then (truth, hypothesis_to_box item :: boxes )
          else raise (IncorrectProof(i))
        | Fresh_Form(_, form) ->
          if _proof l2 (form::truth) boxes terms
          then (truth, hypothesis_to_box item :: boxes )
          else raise (IncorrectProof(i))
    in
    let _ = List.foldi l ~init:(known_truth,boxes)
        ~f:(fun i (truth, boxes) item -> is_item_OK i truth item boxes terms)
    in true

  let proof ?(known_truth=[]) outx p terms  =
    try _proof p known_truth [] terms with
      IncorrectProof(msg) ->
      Out_channel.output_string outx @@ "  Błąd w dowodzie w linii numer " ^  string_of_int msg ^ "\n";
      false
end