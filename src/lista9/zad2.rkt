#lang racket

(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))

(define (branch-struct branch) (cdr branch))
(define (branch-length branch) (car branch))
(define (left-branch struct)  (car struct))
(define (right-branch struct)  (cdr struct))

(define (left-struct struct) (branch-struct (car struct)))
(define (right-struct struct) (branch-struct (cdr struct)))

(define (sum-struct struct ) (cond 
    [ (number? struct) struct ]
    [ else (+   (sum-struct (left-struct struct))
                (sum-struct (right-struct struct)))]))

(let ([x (mk-mobile (mk-branch 11 2) (mk-branch 12 3) )]) (sum-struct x) ); 5


(define (balanced? struct ) (cond
    [ (number? struct ) #t]
    [ else  (and (balanced? (left-struct struct ))
                 (balanced? (right-struct struct))
                 (let* ([lsum (sum-struct (left-struct struct))]
                        [rsum (sum-struct (right-struct struct))]
                        [l_b (branch-length (left-branch struct))]
                        [r_b (branch-length (right-branch struct))])
                        (equal? (* lsum l_b) (* rsum r_b)))) ] ))

(let ([x (mk-mobile (mk-branch 11 2) (mk-branch 12 3) )]) (balanced? x) ); #f
(let ([x (mk-mobile (mk-branch 3 2) (mk-branch 2 3) )]) (balanced? x) ); #t    