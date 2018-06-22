#lang racket

;;;;                  quote
;(quote (1 2 3 4))              ; => '(1 2 3 4)
;(quote (+ 3 4))                ; => '(+ 3 4)
;(quote ("to" "jest" "lista"))  ; => '("to" "jest" "lista")
;'(+ 3 4)                       ; => '(+ 3 4)
;'"napis"                       ; => "napis"
;"napis"                        ; => "napis"
;'((+ 1 2) 5)                   ; => '((+ 1 2) 5)
;(cons (+ 1 2) 5)               ; => '(3 . 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;    Pary i listy

;(cons 1 2)                      ; => '(1 . 2)
;'(1 . 2)                        ; => '(1 . 2)

;'()                             ; => '() lista pusta
;(list)                          ; => '() lista pusta
(define l1 (cons 'a (cons 'b (cons 'c '())))) ;'(a b c) lista właściwa
(define l2 '(a b c))                          ;'(a b c) lista właściwa
(define l3 (list 'a 'b 'c))                   ;'(a b c) lista właściwa
;(equal? l1 l2)                  ; => #t
;(eq? l1 l2)                     ; => #f
;(equal? l1 l3)                  ; => #t
;(eq? l1 l3)                     ; => #f
;(cons 'a (cons 'b (cons 'c 'd)))  ;'(a b c . d) lista niewłaściwa


;;;;                   pary niemodyfikowalne (Racket)
(define pn (cons 1 2))
;pn                     ; => '(1 . 2)
;(print pn) (newline)   ; => '(1 . 2)
;(write pn) (newline)   ; => '(1 . 2)
;(displayln pn)         ; => '(1 . 2)
;(pair? pn)             ; #t
;(car pn)               ; => 1
;(cdr pn)               ; => 2

;;;;                   pary modyfikowalne  (Racket)
(define mp (mcons 1 2))
;mp                    ; => (mcons 1 2)
;(print mp) (newline)  ; => (mcons 1 2)
;(write mp) (newline)  ; => {1 . 2}  nawiasy klamrowe!
;(displayln mp)        ; => {1 . 2}  nawiasy klamrowe!
;(mpair? mp)           ; #t
;(mcar mp)             ; => 1
;(mcdr mp)             ; => 2
;(set-mcar! mp -1)  
;(set-mcdr! mp -2)  
;mp                    ; => (mcons -1 -2)

;;;;;;;;;;;;;;;;;; współdzielenie i tożsamość
;(eq? 'a 'a)               ; => #t
(define xs1 (list 'a 'b))
;xs1                      ; => '(a b)
(define ys1 xs1)
;ys1                      ; => '(a b)
;(eq? xs1 ys1)             ; => #t
(define ys2 (cons (car xs1) (cdr xs1)))
;(eq? xs1 ys2)             ; => #f
;(equal? xs1 ys2)          ; => #t
(define xs2 (list 'c 'd))
(define zs (append xs1 xs2))
;(eq? xs2 (cdr (cdr zs)))  ; => #t

;;;;;;;;;;;;;;;;;;;;;;;;;;  Przykład: stos 

(define make-stack 
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
          [(eqv? msg 'empty?) (null? ls)]
          [(eqv? msg 'push!) (set! ls (cons (car args) ls))]
          [(eqv? msg 'top) (if (null? ls) (error 'empty_stack) (car ls))]
          [(eqv? msg 'pop!) (if (null? ls) (set! ls '()) (set! ls (cdr ls)))]
          [else (error 'unrecognized_stack_operation)])))))

(define stack  (make-stack))
;(stack 'empty?)     ; => #t
;(stack 'pop!)       ; => 
;(stack 'push! 1)
;(stack 'push! 2)
;(stack 'top)        ; => 2
;(stack 'pop!)
;(stack 'top)        ; => 1
;(stack 'pop!)
;(with-handlers ([exn:fail? (lambda (exn) exn)]) (stack 'top))
;(with-handlers ([exn:fail? (lambda (exn) exn)]) (stack 'hop))


;;;;;;;;;;;;;;;   Struktury
(struct point (x (y #:mutable)) #:transparent)
(define my-point (point 1 2))
;my-point                    ; => (point 1 2)
;(point? my-point)           ; => #t
;(point-x my-point)          ; => 1   
;(point-y my-point)          ; => 2
;(set-point-y! my-point -2) 
;(point-y my-point)          ; => -2


;; porównaj z rekordami w OCamlu, wykład 4, str. 27-29
(struct complex-o (re im))
(define c-o (complex-o 2. 3.))
;c-o                      ; => #<complex-o
;(complex-o-re c-o)       ; => 2.0

(struct complex (re im) #:transparent)
(define c (complex 2. 3.))
;c                        ; => (complex 2.0 3.0)
(define (add_complex c1 c2) 
  (complex (+ (complex-re c1) (complex-re c2)) (+ (complex-im c1) (complex-im c2))))
;(add_complex c c)        ; => (complex 4.0 6.0)

(struct aRec (p1 re) #:transparent)
(define rec1 (aRec #f 5.6))
;rec1                     ; => (aRec #f 5.6)
(define rec2 (struct-copy aRec rec1 [re 0.]))
;rec2                     ; => (aRec #f 0.0)

;; porównaj z rekordami w OCamlu, wykład 6, str. 18
(struct punkt (wx (wy #:mutable)) #:transparent)
(define p (punkt 1. 0.))
(set-punkt-wy! p 3.)
;p                        ; => (punkt 1.0 3.0) 
;(punkt? p)               ; => #t

;; porównaj z listami cyklicznymi w OCamlu, wykład 6, str. 30-33
(struct lnode (item (next #:mutable))  #:transparent)

(define (mk_circular_list e)
  (letrec ([x (lnode e '())]) (set-lnode-next! x x) x))

(define (insert_head! e cl)
  (letrec ([x (lnode e (lnode-next cl))]) (set-lnode-next! cl x) cl))


(define (insert_tail! e cl)
  (letrec ([x (lnode e (lnode-next cl))]) (set-lnode-next! cl x) x))

(define (first cl)
  (lnode-item (lnode-next cl)))

(define (last cl)
  (lnode-item cl))

(define (elim_head cl)
  (begin (set-lnode-next! cl (lnode-next (lnode-next cl))) cl))

(define cl1
  (let ([l (mk_circular_list 1)]) (foldr insert_tail! l (list 5 4 3 2))))
;cl1         ; => #0=(lnode 5 (lnode 1 (lnode 2 (lnode 3 (lnode 4 #0#)))))
;(first cl1) ; => 1
;(last cl1)  ; => 5

(define cl2
  (let ([l (mk_circular_list 1)]) (foldr insert_tail! l (list 5 4 3 2))))
;cl2  ; => #0=(lnode 5 (lnode 1 (lnode 2 (lnode 3 (lnode 4 #0#)))))

;(eq? cl1 cl2)     ; => #f
;(eq? cl1 cl1)     ; => #t
;(eqv? cl1 cl2)    ; => #f
;(equal? cl1 cl2)  ; => #t


;;;;;;;;;;;;;;;;;;;  Dopasowanie do wzorca

(define (fpat v)
  (match v
    [(cons x x) "variable may occur in a pattern more than once" ]
    [(cons x y) (cons y x)] 
    [(point x y) (point y x)]
    [ _ 'wildcard]))

;(fpat '(2 . 2))      ; => "variable may occur in a pattern more than once"
;(fpat '(2 . 3))      ; => '(3 . 2)
;(fpat (point 2  3))  ; => (point 3 2)
;(fpat 5)             ; => 'wildcard
