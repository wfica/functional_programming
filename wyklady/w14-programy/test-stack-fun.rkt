#lang racket
;; porównaj ze stosem w OCamlu, wykład 7, str. 13-15
(require "stack-fun.rkt")
;; Plik "stack-fun.rkt" musi być w tym samym folderze. Można też używać względnych ścieżek.

(empty? (create))                         ; => #t
(pop (create))                            ; => #<stack> 
(push 2 (push 1 (create)))                ; => #<stack>
(top (push 2 (push 1 (create))))          ; => 2
(top (pop (push 2 (push 1 (create)))))    ; => 1
(pop (pop (push 2 (push 1 (create)))))    ; => #<stack>
;(top (create))
(with-handlers ([exn:fail? (lambda (exn) exn)]) (top (create)))
;; => (exn:fail "error: empty_stack" #<continuation-mark-set>)
