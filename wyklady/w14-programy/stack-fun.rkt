#lang racket
;; porównaj ze stosem w OCamlu, wykład 7, str. 13-15
(provide create empty? push top pop)

(struct stack (ls)) 
(define (create) 
  (stack '()))
(define (empty? ls) 
  (null? (stack-ls ls)))
(define (push v ls) 
  (stack (cons v (stack-ls ls))))
(define (top ls) 
  (if (null? (stack-ls ls)) (error 'empty_stack) (car (stack-ls ls))))
(define (pop ls) 
  (if (null? (stack-ls ls)) (stack '()) (stack (cdr (stack-ls ls)))))
  