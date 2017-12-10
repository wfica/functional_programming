{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let task_name = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }
  | "goal"    { GOAL }
  | "proof"   { BEGIN_PROOF }
  | "end."    { END_PROOF }
  | task_name { TASK_NAME (Lexing.lexeme lexbuf) }
  | id        { VARIABLE (Lexing.lexeme lexbuf) }
  | 'T'       { TRUE }
  | 'F'       { FALSE }
  | "/\\"     { AND }
  | "\\/"     { OR }
  | '~'       { NOT }
  | "=>"      { IMPL }
  | "<=>"     { IFF }
  | ':'       { COLON }
  | ';'       { SEMI_COLON }
  | '('       { LEFT_BRACK }
  | ')'       { RIGHT_BRACK }
  | '['       { LEFT_SQUARE_BRACK }
  | ']'       { RIGHT_SQUARE_BRACK }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof       { EOF }
