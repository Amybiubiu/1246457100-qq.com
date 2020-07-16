#lang sicp
;;;2.3

;;2.53

(list 'a 'b 'c)
(list (list 'george))
(cdr  '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(define a (list 2 3))
(cdr a)

;;2.54

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))   ;注意不能直接用symbol？
         (eq? a b))
        ((and (pair? a) (pair? b))
         (cond ((eq? (car a) (car b))
                (equal? (cdr a) (cdr b)))
               (else false)))
        (else false)))
(equal? '(this is) '(this is))
(equal? '(this (is)) '(this is))
     
;;2.55
(car ''abcdefg)

;;2.56
(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

#|(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiation? exp)
         (make-product (derive (base exp) var)
                       (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))))))
|#
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))

(define (base exp)
  (cadr exp))
(define (exponent exp) (caddr exp))


;;2.57
#|(define (augent a)
  (accumulate make-sum 0 (cddr a))) 
(define (multiplicand a)
  (accumulate make-product 1 (cddr  a)))

(define (accumulate op init sequence)
  (cond ((null? sequence) init)
        (else (op (car sequence)
                            (accumulate op init (cdr sequence))))))
|#
;;2.58
;a)
(define (make-sum a1 a2) (list a1 '+ a2));此处加一个化简更好
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (sum? exp) (and (pair? exp) (eq? (cadr exp) '+)))

;b)可能会有些难 今晚困了 带不动

;;2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (union-set-a s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (if (element-of-set? (car s1) s2)
                  (union-set-a (cdr s1) s2)
                  (cons (car s1)
                        (union-set-a (cdr s1) s2))))))
(union-set-a '(3 1 4) '(2 3 4))
  
;;2.60 题目太多了 而且大概方法是一样的

;;2.61
(define (adjoin-set e set)
  (cond ((> e (car set))
         (cons (car set) (adjoin-set e (cdr set))))
        ((< e (car set))
         (cons e set))
        ((= e (car set))
         set)))
(adjoin-set 2 '(1 3 4 5))
(adjoin-set 2 '(1 2))
;(adjoin-set 3 '()) 需要加个if判断一下 

;;2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ( (x1 (car s1)) (x2 (car s2)) )
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr s1)
                                        (cdr s2))))
                      ((< x1 x2)
                       (cons x1
                             (union-set (cdr s1)
                                        s2)))
                      ((> x1 x2)
                       (cons x2
                             (union-set s1
                                        (cdr s2)))))))))
(union-set '(1 3 4) '(2 3 4))

;;2.63
#|
a)
tree-to-list-1:

tree-to-list-2:

相同
b)
第二种通过迭代的方式增长的更慢
|#

;;2.64 有点复杂 要午睡了 呜呜呜 list-to-tree

;;2.65 用了各种组合 大大清晰‘简化’了操作
(define (union-tree t1 t2)
  (list-to-tree (union-set (tree-to-set t1)
                           (tree-to-set t2))))

(define (intersection-tree t1 t2)
  (list-to-tree (intersection-set (tree-to-set t1)
                                  (tree-to-set t2))))

;;2.66

;;2.67

;;2.68

;;2.69

;;2.70

;;2.71

;;2.72