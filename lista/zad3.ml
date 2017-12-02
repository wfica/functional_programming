module type SORT = functor (Order:ORDTYPE) -> 
sig
  type elem = Order.t 
  val sort : elem list -> elem list
end

module QueueSort :SORT = 
  functor (Order:ORDTYPE) ->
  struct
    type elem = Order.t
    module PQ = PQueue(Order) 
    open PQ
    let list_to_queue l  =
      List.fold l ~init:empty ~f:(fun acc x -> insert acc x) 
    let queue_to_list q = 
      let rec take_out q acc =
        match peek q with
        | None -> acc
        | Some(hd,tl) -> take_out tl (hd::acc)
      in take_out q []
    let sort l = 
      list_to_queue l |> queue_to_list 
  end
;;