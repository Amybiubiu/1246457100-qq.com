#lang sicp
;;;2.2

;;2.17
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))
#;(last-pair (list 23 72 149 34))
#;(list 23 72 149 34)

;;2.18
;对于cons而言，感觉迭代比较适合用来向前追加，递归比较适合向后连接
;看2.21 
(define (reverse list)
  (define (iter lis res)
    (if (null? lis)
        res
        (iter (cdr lis)
              (cons (car lis) res))))
  (iter list '()))
#;(reverse (list 1 4 9 16 25))

;;2.19
(define (no-more? list)
  (null? list))
(define (first-denomination list)
  (car list))
(define (except-first-denomination list)
  (cdr list))

;;2.20
(define (same-parity x . xs)
  (filter (if (odd? x)
              odd?
              even?)
          (cons x xs)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
#;(same-parity 1 2 3 4 5 6 7)
#;(same-parity 2 3 4 5 6 7)


;;2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
#;(square-list (list 1 2 3))

;;2.22
;额 那样写迭代当然是反的了
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter  items nil))
#;(square-list-iter (list 1 2 3))
#;(((() . 1) . 4) . 9) ;原来这个东西叫序对？

;;2.23 写在书上了我

;;2.24
#;(list 1 (list 2 (list 3 4)))
(define one (list 1 3 (list 5 7) 9))
(define two (list (list 7)))
(define three (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

#;(car (cdr (car (cdr (cdr one)))))
#;(car (car two))
#;(car (cdr (cdr (cdr (cdr (cdr (cdr three))))))) ;error
#;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr three))))))))))))

;;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
#;(append x y)
#;(cons x y)
#;(list x y)

;;2.27  感觉有些难？到了这部分？pair是什么？不会……
(define (deep-reverse li) 
   (cond ((null? li) '()) 
         ((not (pair? li)) li) 
         (else (append (deep-reverse (cdr li))  
                       (list (deep-reverse (car li)))))))
#;(deep-reverse '(2 (3 4) (6 7)))
#;(append '(4 3) '((1 2)))

;;2.28
(define (fringe li)
  (cond ((null? li) '())
        ((not (pair? li)) (list li))  ;外层套一个list
        (else (append (fringe (car li))
                      (fringe (cdr li))))))
#;(fringe '((1 2) (3 4)))

;;2.29 就算了 题目好长 我好蠢

;;2.30
(define (square-tree li)
  (cond ((null? li) '())
        ((not (pair? li)) (square li))
        (else (cons (square-tree (car li))
                    (square-tree (cdr li))))))
(define (square-tree-m li)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)   ;注意，仍需要在map内取pair
             (square-tree-m sub-tree)
             (square sub-tree)))
       li))

;;2.31
(define (tree-map fun tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fun sub-tree)
             (fun sub-tree)))
       tree))
;;2.32 I don't know ;画树形图比较好理解
(define nil '())
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))
#;(subsets (list 1 2 3))

;;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))  ;仿佛意识到自己是zz
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;2.34
(define (horner-eval x coefficent-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficent-sequence))
#;(horner-eval 2 (list 1 3 0 5 0 1))

;;2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   t)))
#;(count-leaves '((1 2) (3 4)))

;;2.36 再次感觉到了难度 哭
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (s) (car s)) seqs))
            (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))
;原来是map 害没有想到

;;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (t) (dot-product v t)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))
;不用for循环 而用 高阶 抽象 组合 是这样的感觉啊

;;2.38

;;2,39

;;2.40

;;2.41

;;2.42

;;2.43

;;;2.2.4 感觉这部分会更难

;;2.44

;;2.45

;;2.46

;;2.47

;;2.48

;;2.49

;;2.50

;;2.51

;;2.52