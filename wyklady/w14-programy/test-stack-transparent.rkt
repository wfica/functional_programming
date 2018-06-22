#lang racket
(require "stack-transparent.rkt")
;; porównaj ze stosem w OCamlu, wykład 7, str. 11
;; Plik "stack-transparent.rkt" musi być w tym samym folderze. Można też używać względnych ścieżek.

(empty? (create))                         ; => #t
(pop (create))                            ; => '()
(push 2 (push 1 (create)))                ; => '(2 1)
(top (push 2 (push 1 (create))))          ; => 2
(top (pop (push 2 (push 1 (create)))))    ; => 1
(pop (pop (push 2 (push 1 (create)))))    ; => '()
;(top (create))
(with-handlers ([exn:fail? (lambda (exn) exn)]) (top (create)))
;; (exn:fail "error: empty_stack" #<continuation-mark-set>)