#lang racket

;; functions

(define (id x) x)

(define (compose f g) (lambda (x) (f (g x))))

;; continuations

(define *depth* 0)
(define *max-depth* 0)
(define *ticks* 0)
(define *answer* '())

(define (build-cont f)
  (set! *depth* (+ *depth* 1))
  (set! *max-depth* (max *depth* *max-depth*))
  f)

(define (bounce thunk)
  (set! *ticks* (+ *ticks* 1))
  (if (procedure? thunk)
      (bounce (thunk))
      thunk))

(define (apply-cont k val)
  (lambda ()
    (set! *depth* (- *depth* 1))
    (k val)))

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
; CPS style
(define (concatK l1 l2 k)
  (if (null? l1)
      (apply-cont k l2)
      (concatK (cdr l1) l2 (build-cont (lambda (l3)
                                         (apply-cont k (cons (car l1) l3)))))))

; leaves
; CPS style
(define (leavesK x k)
  (cond ((leaf? x)
         (apply-cont k (cons (leaf->label x) '())))
        ((node? x)
         (leavesK (node->left x) (build-cont (lambda (l)
                                               (leavesK (node->right x) (build-cont (lambda (r)
                                                                                      (concatK l r k))))))))))

; same?
; CPS style
(define (same?K a b k)
  (if (null? a)
      (apply-cont k (null? b))
      (if (equal? (car a) (car b))
          (same?K (cdr a) (cdr b) k)
          (apply-cont k #f))))

; samefringe
; CPS style
(define (sameFringeK a b k)
  (leavesK a (build-cont (lambda (l1)
                           (leavesK b (build-cont (lambda (l2)
                                                    (same?K l1 l2 k))))))))

; samefringe
; direct style
(define (sameFringe a b)
  (set! *depth* 0)
  (set! *max-depth* 0)
  (set! *ticks* 0)
  (bounce
   (sameFringeK a b (build-cont (lambda (answer)
                                  (set! *answer* answer)
                                  (display " max-depth: ") (display *max-depth*) (newline)
                                  (display " ticks:     ") (display *ticks*) (newline)
                                  '()))))
  *answer*)

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
