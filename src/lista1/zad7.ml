let id = fun x -> x;;

let int_id = fun (x : int) -> x;; 

let apply f g x = f  (g x);;
apply (fun x -> x + x ) (fun x -> x * x) 3;;

let rec f x = f x;;
let f2 x = List.hd [];;
