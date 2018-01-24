#lang racket
;; porównaj ze stosem w OCamlu, wykład 7, str. 11
(provide create empty? push top pop)


(define (create) '())
(define (empty? ls) (null? ls))
(define (push v ls) (cons v ls))
(define (top ls) (if (null? ls) (error 'empty_stack) (car ls)))
(define (pop ls) (if (null? ls) '() (cdr ls)))
  