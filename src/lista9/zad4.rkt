#lang racket

(struct queue (in out) #:mutable #:transparent) 

(define (make-queue) (queue empty empty ) )
(define (queue-empty? q)  (and (empty? (queue-out q )) (empty? (queue-in q))))
(define (queue-move q) 
    (set-queue-out! q (append  (queue-out q) (reverse (queue-in q))))
    (set-queue-in! q empty) ) 
(define (queue-add q v) ( set-queue-in! q  (cons  v (queue-in q)) ))
(define (queue-pop q) 
    (if (queue-empty? q) 
        (error "empty queue!")
        (if ( empty? (queue-out q)) 
            (begin (queue-move q ) (queue-pop q) )
            (let* (
                [lst (queue-out q)]
                [ans (car lst)]
                [rest (cdr lst)])
                (begin (set-queue-out! q rest) ans ) ))))


(define q (make-queue))
q
(queue-empty? q)
(queue-add q 10)
(queue-empty? q)
q
(queue-add q 20)
(queue-move q)
(queue-add q 30)
q
(queue-pop q)
(queue-add q 40)
(queue-pop q)
q
