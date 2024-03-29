#lang racket

(define (count-change change coins)
  (cond [(empty? coins) 0]
        [(zero? change) 1]
        [(negative? change) 0]
        [else
          (+ (count-change change (cdr coins))
             (count-change (- change (car coins)) coins))]))


(count-change 100 '(1 5 10 25 50))
(count-change 6 '(2 3 10))