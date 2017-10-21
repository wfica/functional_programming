let plus1  = fun x y -> x + y;;
plus1 10 11;;

let plus2 x y = x + y;;
plus2 1 1;;

let plus3 = function x -> function y -> x + y;;
plus3 12 33;;

let plus_3 = plus3 3;;
plus_3 10;;