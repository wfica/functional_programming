#lang racket

(define (deriv_ expr var)
    (match expr  
        [(list '+ a b)  
            (let ([x (deriv_ a var)] 
                  [y (deriv_ b var)]) 
                                                    (list '+ x y) )]
        [(list '*  a b) 
            (let ([x (deriv_ a var)] 
                  [y (deriv_ b var)]) 
            (list '+ (list  '* x b) (list '* a y ) ) )]
        [var2 (if (equal? var var2) 1 0)]))

(define (normalize expr) 
    (match expr
        [(list '+ 0 a) (normalize a)]
        [(list '+ a 0) (normalize a)]
        [(list '+ a b) (let ([x (normalize a)] [y (normalize b)]) (list '+ x y) )]
        [(list '* a 0) 0]
        [(list '* 0 a) 0]
        [(list '* 1 a) a]
        [(list '* a 1) a]
        [(list '* a b) (let ([x (normalize a)] [y (normalize b)]) (list '* x y) )]
        [var var]))


(define (normilizer expr)
    (let ([x (normalize expr) ])
        (if (equal? x expr) x (normilizer x))
    )
)

(define (deriv expr var) 
    (let ([res (deriv_ expr var)]) (normilizer res) ))

(deriv '(* z x) 'x )
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y) 'x)