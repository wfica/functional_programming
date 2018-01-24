#lang racket
;; porównaj ze stosem w OCamlu, wykład 7, str. 19-20
(require "stack-mut-list.rkt")
;; Plik "stack-mut-list.rkt" musi być w tym samym folderze. Można też używać względnych ścieżek.

(define s (create))
(empty? s)       ; => #t
(pop! s)                             
(push! 1 s)
(push! 2 s)         
(top s)          ; => 2
(pop! s)
(top s)          ; => 1
(pop! s)
(with-handlers ([exn:fail? (lambda (exn) exn)]) (top s))
;; => (exn:fail "error: empty_stack" #<continuation-mark-set>)