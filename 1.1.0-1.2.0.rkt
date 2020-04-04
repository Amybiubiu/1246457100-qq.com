#lang sicp
;;1.1
(define a 3)
(define b (+ a 1))
(= a b)
(cond (( = a 4) 6)
      (( = b 4) (+ 6 7 a))
      (else 25))

;;1.2
(/ (+ 5 4 (- 2
             (- 3 (+ 6
                     (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
;;1.3
(define (addTwoMax x y z)
  (cond ( (and (> x y) (> z y)) (+ x z))
        ( (and (> y x) (> z x)) (+ y z))
        ( (and (> x z) (> y z)) (+ x y))))
(addTwoMax 2 3 4)
;;1.4
(define (a-puls-abs-b a b)
  ((if (> b 0) + -) a b))
;;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
#;(test 0 (p))

;;1.6
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) ;iteractions disabled,out of memory.
       guess
      (sqrt-iter (improve guess x) x)))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
#;(sqrt 4)

;;1.7
(define (new-good-enough? old new)
  (> 0.01 (/ (abs (- old new))
             old)))
(define (new-sqrt-iter guess x)
  (let ((new-guess (improve guess x)))
    (if (new-good-enough? guess new-guess)
        new-guess
        (new-sqrt-iter new-guess x))))
#;(new-sqrt-iter 1 0.00009)
;;1.8
(define (new-improve guess x)
  (/ (+ (/ x (square guess)) guess guess)
     3))
(new-sqrt-iter 1 9)