#lang racket

;; functions

(define (id x) x)

(define (compose f g) (lambda (x) (f (g x))))

;; continuations

;; lists

(define pair*? pair?)

(define cons* cons)

(define car* car)

(define cdr* cdr)

(define list* list)

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

; concat*
; direct style
(define (concat* l1 l2)
  (let ((l1* (l1)))
    (display "l1* ")(display l1*)(newline)
    (if (null? l1*)
        l2
        (lambda () (cons* (car* l1*) (concat* (cdr* l1*) l2))))))

; leaves
; direct style
(define (leaves x)
  (let ((x* (x)))
    (display "x* ")(display x*)(newline)
    (cond ((leaf? x*)
           (lambda () (cons* (lambda () (leaf->label x*)) (lambda ()'()))))
          ((node? x*)
           (concat* (leaves (lambda () (node->left x*)))
                    (leaves (lambda () (node->right x*))))))))

; same?
; direct style
(define (same? a b)
  (let ((a* (a))
        (b* (b)))
    (display "a* ")(display a*)(newline)
    (display "b* ")(display b*)(newline)
    (if (and (pair*? a*) (pair*? b*))
        (and (same? (car* a*) (car* b*))
             (same? (cdr* a*) (cdr* b*)))
        (equal? a* b*))))

; samefringe
; CPS style

; samefringe
; direct style
(define (sameFringe a b)
  (same? (leaves (lambda () a))
         (leaves (lambda () b))))

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

(define size 10)

(define (time f)
  (let* ((start-time (current-milliseconds))
         (result (f))
         (end-time (current-milliseconds))
         (elapsed-time (- end-time start-time)))
    (display " elapsed time: ") (display (/ elapsed-time 1000.0)) (newline)
    result))

(define (test name expected left right)
  (display "running ") (display name) (newline)
  (time (lambda ()
          (let ((actual (sameFringe left right)))
            (if (eq? expected actual)
                (begin (display "*SUCCESS*") (newline))
                (begin (display "*FAILURE*") (newline))))))
  (newline))


(test "same leaves"       #t   (Leaf 1)                                                      (Leaf 1))
(test "different leaves"  #f   (Leaf 1)                                                      (Leaf 2))
(test "same trees"        #t   (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))                      (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(test "rightish/rightish" #f   (Node (Leaf 1) (Node (generateRightishTree size) (Leaf 0)))   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
(test "leftish/leftish"   #f   (Node (Leaf 2) (Node (generateLeftishTree size)  (Leaf 0)))   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "rightish/leftish"  #f   (Node (Leaf 3) (Node (generateRightishTree size) (Leaf 0)))   (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "leftish/rightish"  #f   (Node (Leaf 4) (Node (generateLeftishTree size)  (Leaf 0)))   (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
