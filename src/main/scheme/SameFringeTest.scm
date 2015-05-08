#lang racket
(require "Tree.scm")
(require "SameFringe.scm")
;(require "SameFringeL.scm")
;(require "SameFringeK.scm")
;(require "SameFringeT.scm")

;; tree generators
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

;; test runner
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


(define yes #t)
(define no  #f)

(define size 1000)

;; tests
(display "SameFringe [Scheme] ") (display implStyle) (newline)

(test "same leaves" yes
      (Leaf 1)
      (Leaf 1))

(test "different leaves" no
      (Leaf 1)
      (Leaf 2))

(test "same trees" yes
      (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
      (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(test "rightish/rightish different first" no
      (Node (Leaf 1) (Node (generateRightishTree size) (Leaf 0)))
      (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

(test "leftish/leftish different first" no
      (Node (Leaf 2) (Node (generateLeftishTree size)  (Leaf 0)))
      (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))

(test "rightish/leftish different first" no
      (Node (Leaf 3) (Node (generateRightishTree size) (Leaf 0)))
      (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))

(test "leftish/rightish different first" no
      (Node (Leaf 4) (Node (generateLeftishTree size)  (Leaf 0)))
      (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

(test "rightish/rightish" yes
      (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))
      (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

(test "leftish/leftish" yes
      (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
      (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "rightish/leftish" yes
      (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))
      (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "leftish/rightish" yes
      (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
      (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

;      int size = 1000 * i;
;      test("leftish/leftish " + size, yes, //
;           Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))),
;           Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))));

(do ((i 0 (+ i 1)))
     ((> i 100))
  (let ((size (* 1000 i)))
    (test (string-append "leftish/leftish " (number->string size)) yes
          (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
          (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))))


