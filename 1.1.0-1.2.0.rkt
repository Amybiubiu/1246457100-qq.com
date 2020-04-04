#lang sicp
;;1.1
(define a 3)
(define b (+ a 1))
(= a b)
(cond (( = a 4) 6)
      (( = b 4) (+ 6 7 a))
      (else 25))

;;1.2