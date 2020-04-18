#lang sicp
;;(#%require sicp-pict)
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
        (else (+ (pascal (- x 1)
                         (- y 1))
                 (pascal (- x 1)
                         y)))))
#;(pascal 4 2)
;;1.13 I plan to give up

;;1.14

;;1.15
;a) 5
;b) Pace:log a,Space:log a

;;1.16
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter producer)
  (cond ((= counter 0) producer)
        ((even? counter) (expt-iter (* b b)
                                    (/ counter 2)
                                    producer))
        (else (expt-iter b
                         (- counter 1)
                         (* producer b)))))
(define (even? n)
  (= (remainder n 2) 0))

;;1.17
;递归和迭代混合版 泪
(define (double x)
  (* 2 x))
(define (halve x)
  (/ x 2))
(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b) (multi (double a) (halve b)))
        ;R-> ((even? b) (double (multi a (halve b)))) 
        (else (+ a (multi a
                      (- b 1))))))
#;(multi 3 4)

;;1.18
(define (multi-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (multi-iter (double a)
                               (halve b)
                               product))
        (else (multi-iter a
                          (- b 1)
                          (+ product a)))))
#;(multi-iter 3 4 0)

;;1.19  没明白题目，看了resolution后才明白题目的意思

;;1.20
;正则序  4  ;R-> 14+4
;应用序  4

;;1.21

;;1.22 这题用命令式语言挺好写的，换成用声明式好不习惯,吐血中

(define (search-for-primes a n)
  (search-iter a n 0))
(define (search-iter a n count)
  (if (= count n)
      (display "over")
      ((timed-prime-test a)
       (search-iter (next-odd a) n (+ count 1)))))
(define (next-odd num)
  (if (odd? num)
      (+ 1 num)
      (+ 2 num)))
(define (timed-prime-test n) 1)
;;1.23 怎么这几题都这么长 可怕

;;1.24

;;1.25

;;1.26

;;1.27

;;1.28

      
