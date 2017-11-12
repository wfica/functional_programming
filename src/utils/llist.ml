
type 'a llist = LNil | LC of 'a * 'a llist Lazy.t;;


let sum_pref_stream stream =
  let rec _spref_stream s acc =
    match s with 
    | LNil -> LNil
    | LC(hd, lazy tl) -> LC(acc +. hd, lazy ( _spref_stream tl (acc +. hd ) ) )
  in _spref_stream stream 0.
;;