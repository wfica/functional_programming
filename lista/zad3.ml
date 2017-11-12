open Core.Std;;

let q = Queue.create () ;;
let _ = Queue.enqueue q 9 ;;
let _ = Queue.enqueue q 19 ;;
let _ = Queue.enqueue q 119 ;;
let _ = Queue.dequeue q ;;
let x = Queue.dequeue q;;
