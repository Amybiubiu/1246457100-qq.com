#lang sicp
;;1.9
;; first: recursion,second: iteration

;;1.10
#|
(A 1 10) 2^10
(A 2 4) 2^16
(A 3 3) 2^16
(f n) -> 2n
(g n) -> 2^n
(h n) -> 2^(2^n) X -> 对2求n次幂 √
|#
;;1.11
;recursion
(define (func n)
  (if (< n 3) n
      (+ (func (- n 1)) (* 2 (func (- n 2))) (* 3 (func (- n 3))))))
#;(func 3)
;iteration
(define (f n)
  (f-iter 2 1 0 0 n))
(define (f-iter a b c i n)
  (if (= i n)
      c
      (f-iter
       (+ a (* 2 b) (* 3 c))
       a
       b
       (+ i 1)
       n)))
#;(f 3)
;;1.12
;recursion
(define (pascal x y)
  (cond ((= y 0) 1)
        ((= x y) 1)
        (else
;;1.13 I plan to give up