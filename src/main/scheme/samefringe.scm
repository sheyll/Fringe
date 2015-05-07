#lang racket

;; functions

(define (id x) x)

(define (compose f g) (lambda (x) (f (g x))))

;; binary tree

; leaf
(define Leaf id)

(define leaf? (compose not pair?))

(define leaf->label id)

; node
(define Node cons)

(define node? pair?)

(define node->left car)

(define node->right cdr)

; concat
; direct style
(define (concat l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat (cdr l1) l2))))

; leaves
; direct style
(define (leaves x)
  (cond ((leaf? x)
         (cons (leaf->label x) '()))
        ((node? x)
         (concat (leaves (node->left x))
                 (leaves (node->right x))))))

; same?
; direct style
(define (same? a b)
  (if (null? a)
      (null? b)
      (if (equal? (car a) (car b))
          (same? (cdr a) (cdr b))
          #f)))

; samefringe
; direct style
(define (sameFringe a b)
  (same? (leaves a)
         (leaves b)))

; generator
(define (generateRightishTree size)
  (define (generate current)
    (if (< current size)
        (Node (Leaf current) (generate (+ current 1)))
        (Leaf size)))
  (generate 0))

(define (generateLeftishTree size)
  (if (> size 0)
      (Node (generateLeftishTree (- size 1)) (Leaf size))
      (Leaf 0)))

;; tests

(define size 1000)

(define (time f)
  (let* ((start-time (current-milliseconds))
         (result (f))
         (end-time (current-milliseconds))
         (elapsed-time (- end-time start-time)))
    (display " elapsed time: ") (display (/ elapsed-time 1000.0)) (newline)
    result))

(define (test name expected left right)
  (display "running test: ") (display name) (newline)
  (time (lambda ()
          (let ((actual (sameFringe left right)))
            (if (eq? expected actual)
                '()
                (begin
                  (display "expected: ") (display expected) (newline)
                  (display "actual:   ") (display actual) (newline)
                  (error "test failure"))))))
  (newline))


(test "same leaves"       #t   (Leaf 1)                                                      (Leaf 1))
(test "different leaves"  #f   (Leaf 1)                                                      (Leaf 2))
(test "same trees"        #t   (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))                      (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(test "rightish/rightish different first" #f   (Node (Leaf 1) (Node (generateRightishTree size) (Leaf 0)))   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
(test "leftish/leftish different first"   #f   (Node (Leaf 2) (Node (generateLeftishTree size)  (Leaf 0)))   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "rightish/leftish different first"  #f   (Node (Leaf 3) (Node (generateRightishTree size) (Leaf 0)))   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "leftish/rightish different first"  #f   (Node (Leaf 4) (Node (generateLeftishTree size)  (Leaf 0)))   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

(test "rightish/rightish" #t   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
(test "leftish/leftish"   #t   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "rightish/leftish"  #t   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "leftish/rightish"  #t   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
