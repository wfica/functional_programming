open Core.Std;;


let sgn_float x = if x >= 0. then 1. else -1.
;;

let next_leibniz k = 
  let sgn = sgn_float k in
  -1. *. sgn /. (sgn /. k +. 2.)
;;

let rec leibniz_from k = LC(k,  lazy (leibniz_from (next_leibniz k) ) )
;;

let leibniz_approx = sum_pref_stream  (leibniz_from 1.) 
;;

let rec ltake ll n =
  match ll, n with
  | LNil, _ -> []
  | _, 0 -> []
  | LC(hd, lazy tl), _ -> hd :: ltake tl (n-1) 
;;

let rec ltake_with_tail ll n =
  match ll, n with
  | LNil, _ -> ([], LNil)
  | xs, 0  -> ([], xs)
  | LC(hd, lazy tail), _ -> 
    let (l, tl) = ltake_with_tail tail  (n-1)
    in (hd::l, tl)
;;


let transform_stream ll f =
  let rec _transform ll f =
    let tail = ltake_with_tail ll 1  |> snd 
    and args = ltake_with_tail ll 3  |> fst in
    LC(f args, lazy( _transform tail f ) )
  in
  _transform ll (fun [a; b; c] -> f a b c )
;;


let euler_transform x y z = z -. (y -. z) *. (y -. z) /. (x -. 2. *. y +. z);;

let faster_conv = transform_stream leibniz_approx euler_transform;;

let test () = 
  let pi_4 = 0.78539816339 in 
  let approx1 =  ltake  leibniz_approx 10000 in
  let approx2 = ltake faster_conv 10000 in
  let args = [0; 9; 99; 999; 9999] in
  [ List.map ~f:(fun i -> List.nth_exn approx1 i -. pi_4 ) args ;
    List.map ~f:(fun i -> List.nth_exn approx2 i -. pi_4 ) args ]

;;

