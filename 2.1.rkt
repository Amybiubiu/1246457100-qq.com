#lang sicp
;;;2.1

;;2.1
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

;;2.2
(define (average a b)
  (/ (+ a b) 2.0))
(define (midpoint-segemnt a)
  (let ((p1 (start-segment))
        (p2 (end-segment)))
    (make-point (average (x-point p1) (x-point p2))
          (average (y-point p2) (y-point p2)))))
;segment
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
;point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

;;2.3
(define (point-distance p1 p2) 
   (sqrt (+ (square (- (x-point p1) (x-point p2))) 
            (square (- (y-point p1) (y-point p2)))))) 
;通用求面积和周长方法
(define (square rect)
  (* (rect-width rect) (rect-height rect)))
(define (premeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))
;用向量定义 还没有去检验合法性
(define (make-rect hori-seg vert-seg)
  (cons hori-seg vert-seg))
(define (hori-rect rect)
  (car rect))
(define (vert-rect rect)
  (cdr rect))
(define (rect-width rect)
  (let ((start-seg (start-segment (hori-rect rect)))
        (end-seg (end-segment (hori-rect rect))))
    (point-distance start-seg end-seg)))
(define (rect-height rect)
  (let ((start-seg (start-segment (vert-rect rect)))
        (end-seg (end-segment (vert-rect rect))))
    (point-distance start-seg end-seg)))    
;用点定义
(define (make-rect-a bottom-left top-right)
   (cons bottom-left top-right))
(define (bottom-left rect)
  (car rect))
(define (top-right rect)
  (cdr rect))
(define (rect-width-a rect)
  (- (x-point (top-right rect))
     (x-point (bottom-left rect))))
(define (rect-height-a rect)
  (- (y-point (top-right rect))
     (y-point (bottom-left rect))))

;;2.4
(define (cdr z)
  (z (lambda (p q) q)))
;代换过程
#;(car (cons x y));->x
#;(car (lambda (m) (m x y)))
;z->(lambda (m) (m x y))
;m->(lambda (p q) p)
#;((lambda (m) (m x y)) (lambda (p q) p))
#;((lambda (p q) p) (x y))
#;x

;;2.5 又是看不懂题目的一道题？？？？
(define (exp base n)
  (define (iter k res)
    (if (= k 0)
        res
        (iter (- k 1) (* res base))))
  (iter n 1))
(define (count-0-remainder-divisions n divisor)
  (define (iter x count)
    (if (= (remainder x divisor) 0)
        (iter (/ x divisor) (+ count 1))
        count))
  (iter n 0))

(define (my-cons a b)
  (* (exp 2 a) (exp 3 b)))
(define (my-car cons)
  (count-0-remainder-divisions cons 2))
(define (my-cdr cons)
  (count-0-remainder-divisions cons 3))

;;2.6 只用过程去定义数据 还可以再令人难以想象一点吗 层层的代换 人都傻了 
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) 
   (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))));代换演算可以得到
(define two (lambda (f) (lambda (x) (f (f x)))))
;看不懂 先跳过？
(define (add a b) 
   (lambda (f) 
     (lambda (x) 
       ((a f) ((b f) x)))))

;;2.7
(define (make-interval x y)
  (cons x y))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))
;;2.8
(define (sub x y)
  (add-interval x
                (make-interval (- (lower-bound y))
                               (- (upper-bound y)))))
;;2.9 pass

;;2.10 pass

;;2.11 ?

;;2.12
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (/ (- (center i) (lower-bound i)) (center i)))

;;2.13 证明题？

;;2.14

;;2.15

;;2.16















  