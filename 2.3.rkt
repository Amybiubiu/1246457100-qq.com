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
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiaion? exp)
         (make-product (derive (base exp) var)
                       (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))))))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car x) '^)))

(define (base exp)
  (cadr exp))
(define (exponent exp) (caddr exp))

;;2.57
(define (augent a)
  (accumulate make-sum 0 (cddr s))) 
(define (multiplicand a)
  (accumulate make-product 1 (cddr  p)))

(define (accumulate op init sequence)
  (cond ((null? sequence) init)
        (else (op (car sequence)
                            (accumulate op init (cdr sequence))))))

;;2.58
;a)
(define (make-sum a1 a2) (list a1 '+ a2));此处加一个化简更好
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (sum? exp) (and (pair? x) (eq? (cadr exp) '+)))

;b)可能会有些难 今晚困了 带不动

;;2.59
