(* zad 1 *)
let a = 5;;
let bref = ref a;;
let cref = bref;;
let a = 3;;
let f x = !bref + a;;
let a = 1;;
bref:=2;;
cref:=3;;

a;;
!bref;;
!cref;;
f 0;;

(* zad 2 *)

type foo = A | B of int | C of int list ;;
let f1 x = match x with   y -> 1;;
let f2 x = match x with A -> 0 | B y -> 1 | C y -> 2;;
let f3 x = match x with A -> 0 | B y -> 1 | C (h::tl) -> 2;;
let f4 x = match x with A -> 0 | _ -> 2 | B _ -> 1;;


(* zad 3 *)

List.fold_left (fun x y -> if y mod 2 = 0 then y::x else x) [] [3;4;5;6] ;;

(* zad 4 *)
let f z y x = z( y x)

(* zad 5 *)

module type S = 
sig 
  type t 
  type s = float 
  val f: s->t 
end 


module M1 : S  with type t = int = 
struct 
  type t = int 
  type s  = float 
  let f x = truncate x 
  let x = 2.4 
end 

module F(M:S with type t = int):
sig 
  type t
  type s = float 
  val f: s->t 
end = 
struct 
  type t = M.t 
  type s = M.s 
  let f = M.f 
end 


module M2 = F(M1)
;;





(* zad 6 *)
type t =  AA of int | B of int | C 
let s = AA 5 
let t = AA 10 
let f x = 
  match x with 
  | AA s -> s + 10 
  | B t -> t + 4 
  | t -> 9 
;;