let rec get (x::xs) n =
  if n == 0 then x else get xs (n-1);;

let ten = 
  let rec enum n = n :: (enum (n + 1) ) in
  get (enum 1)  10;;

