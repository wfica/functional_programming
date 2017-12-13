open Core.Std
open Lexing
open Proof_type
open Natural_deduction


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    []
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let verify index (Task(task_name, _, proof) as task) outx =
  Out_channel.output_string outx @@ string_of_int index ^ ":" ;
  if is_task_valid task = false 
  then Out_channel.output_string outx @@ "  Dowód " ^ task_name ^ " błędny - nie dowodzi celu!\n"
  else if NatDed.proof outx proof 
  then Out_channel.output_string outx @@ "    Dowód " ^ task_name ^ " poprawny :)\n"
  else Out_channel.output_string outx @@ "    Dowód " ^ task_name ^ " błędny!\n" 


let solve tasks outFile = 
  let outx = Out_channel.create outFile in 
  List.iteri tasks ~f:(fun i task -> verify i task outx ) ;
  Out_channel.close outx 

let parse_inFile inFile outFile () =
  let inx = In_channel.create inFile in 
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = inFile };
  let tasks = parse_with_error lexbuf in
  In_channel.close inx ;
  solve tasks outFile

let () =
  Command.basic 
    ~summary:"Proof checker - Wojtek Fica"
    ~readme:(fun () -> "usage:\n arguments: input_file output_file")
    Command.Spec.(empty +> anon ("in" %: file ) +> anon ("out" %: file) )  
    parse_inFile 
  |> Command.run