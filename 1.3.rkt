#lang sicp
;;;1.3

;;1.29
(define (simpson f a b n)
  (define h (/ (- a b) n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k 0)
               (= k n)) y)
          ((odd? k) (* 4 y))
          (else (* 2 y))))
  (* (sum simpson-term 0 inc  n)
       (/ h 3)))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;;1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
      
(define (factorial n)
  (product (lambda (x) x) 1 inc n))
(factorial 3)
(define (getPI n)
  (define (term k)
    (let ((a (if (odd? k)
                 (+ k 1)
                 (+ k 2)))
          (b (if (odd? k)
                 (+ k 2)
                 (+ k 1))))
      (/ a b)))
  (* 4
     (product-iter term 1 inc n)))
(getPI 10)

;;1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum-h term a next b)
  (accumulate (lambda (x y) (+ x y)) 0 term a next b))
(define (product-h term a next b)
  (accumulate (lambda (x y) (* x y)) 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
;;1.33 看不懂题目 先放过

;;1.34
(define (f g)
  (g 2))
(f (lambda (x) (* x x)))
#;(f f)
;;没办法理解最后会变成(2 2)的形式

;;1.35 数学证明题

;;1.36
;;写了大致的在书上，看答案发现平均阻尼能更快的收敛，懒得写代码了

;;1.37
(define (cont-frac n d k)
  (define (cf i)   ;在递归版本中也可以再定义一个函数
    (if (= i k)
        (/ (n i)
           (d i))
        (/ (n i)
           (+ (d i)
              (cf (+ i 1))))))
  (cf 1))
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    result)))))
  (iter (- k 1) (/ (n k) (d k))))

;;1.38
(define (D x)
  (if (= (remainder x 3) 2)
      (* 2 (/ (+ x 1) 3))
      1))  
(cont-frac (lambda (i) 1)
           D
           10)

;;1.39
(define (tan-cf x k)
  (define (n i)
    (if (= n 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))

;;1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))
;;1.41
(define (double f)
  (lambda (x) (f (f x))))
((double inc) 1)
(((double (double double)) inc ) 5) ;5 + 16
;;1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose (lambda (x) (* x x)) inc) 6) ;49
;;1.43
(define square
  (lambda (x) (* x x)))
(define (repeated f n)
  (define (iter i f-r)
    (if (= i 1)
        f-r
        (iter (- i 1) (compose f f-r)))) ;改正版。再一次把迭代和递归混在一起了，导致了error
  (iter n f))
((repeated square 2) 5)
;;1.44
(define dx 0.000001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))
(define (sm-repeat f n)
  (let ((n-time-sm (repeated smooth n)))
    (n-time-sm f)))   ;这个和1.41很相似 难怪你都不怎么会 当然也可能是我困了的原因
((sm-repeat square 10) 5)
;;1.45 题目太长 况且我也困了 可以吗

;;1.46 同上