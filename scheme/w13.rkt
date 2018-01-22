#lang racket

(define factorial
  (lambda (n)
;  (λ(n)
    (if(zero? n)        
      1
      (* n (factorial (sub1 n))))))

;(factorial 5) => 120

(define f1
  (λ(m)(λ(n)(+ m n))))
; ((f1 2) 3) => 5

(define ***
  (λ (n) (* n (* n n))))
; (*** 2)  => 8

(quote symbol) ; => 'symbol lub
'symbol        ; => 'symbol

((let ([y (λ (x) x)])(y y)) 6)      ; rozszerzenie syntaktyczne dla
( ([λ (y)(y y)][λ (x) x]) 6)

(define Y1
  (lambda (f) 
    (let ([W (lambda (x)(lambda (y)((f (x x)) y)))])
      (W W))))

(define fact1
  (Y1 (lambda (g) (lambda (n)
                    (if (zero? n) 1 (* n (g (sub1 n))))))))

; (fact1 5) => 120

(define Y2
  (lambda (f) 
    (let ([W (lambda (x)(f (lambda (y)((x x) y))))])
      (W W))))

(define fact2
  (Y2 (lambda (g) (lambda (n)
                    (if (zero? n) 1 (* n (g (sub1 n))))))))

; (fact2 5) => 120

;;;;;;;;;;;;;;;;;;;;;;; Porównywanie wartości

;(eq? '(1 2 3) '(1 2 3))         ; => #f
;(equal? '(1 2 3) '(1 2 3))      ; => #t
;(equal? 'ident "ident")         ; => #f  
;(symbol=? 'ident "ident")      ; => błąd typu 
;(= 1 1.0)                       ; => #t
;(eq? 1 1.0)                     ; => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Wyjątki

(define (fact n)
  (if (= n 0) 
      1
      (* n (factorial (- n 1)))))
;(fact -5)         => The program ran out of memory.
(define (fact-exn n)
  (if (zero? n)
      1
      (if (> n 0)(* n (fact-exn (- n 1))) (error 'ujemny_argument))))
;(fact-exn -5)         => error: ujemny_argument
;(with-handlers ([exn:fail? (lambda (exn) exn)]) (fact-exn -5))
;    => (exn:fail "error: ujemny_argument" #<continuation-mark-set>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;    Pary i listy

;(cons 1 2)                      ; => '(1 . 2)
;'(1 . 2)                        ; => '(1 . 2)

;'()                             ; => '() lista pusta
;(list)                          ; => '() lista pusta
(define l1 (cons 'a (cons 'b (cons 'c '())))) ;'(a b c) lista właściwa
(define l2 '(a b c))
(define l3 (list 'a 'b 'c))
;(equal? l1 l2)                  ; => #t
;(equal? l1 l3)                  ; => #t
;(cons 'a (cons 'b (cons 'c 'd)))  ;'(a b c . d) lista niewłaściwa

;;;;;;;;;;;;;;;;;;;;;;;;;;; Lista argumentów funkcji

(define f2
  (λ(m n)(+ m n)))
; (f2 2 3) => 5

(define f3
  (λ p (+ (car p) (cadr p))))
; (f3 2 3) => 5

;((lambda (a b c . r) (list a b c r)) 1 2 3 4 5 6) ; => '(1 2 3 (4 5 6))


(define f4
  (lambda (m . r) (+ m (car r))))
;(f4 2 3)   ; => 5

;;;;;;;;;;;;;;;;;;;;;;;; Zmienne modyfikowalne, sekwencje

(define v 5)
v     ; => 5
(set! v 10)
v     ; => 10

((lambda (x) (set! v 11) x) 'wynik)


(begin (set! v 12) 'wynik)       ;  => 'wynik
((lambda () (+ 1 2) 'wynik))     ;  => 'wynik


