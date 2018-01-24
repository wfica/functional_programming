#lang racket
;; porównaj ze stosem w OCamlu, wykład 7, str. 19-20
(provide create empty? push! top pop!)

(struct stack ((ls #:mutable)))
(define (create) 
  (stack '()))
(define (empty? s) 
  (null? (stack-ls s)))
(define (push! v s) 
  (set-stack-ls! s (cons v (stack-ls s))))
(define (top s) 
  (if (null? (stack-ls s)) (error 'empty_stack) (car (stack-ls s))))
(define (pop! s) 
  (if (null? (stack-ls s)) (set-stack-ls! s '()) (set-stack-ls! s (cdr (stack-ls s)))))
  