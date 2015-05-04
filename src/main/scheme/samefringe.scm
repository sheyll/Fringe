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

; direct style concat
(define (concat l1 l2)
  (if(null? l1)
     l2
     (cons (car l1) (concat (cdr l1) l2))))

; direct style leaves
(define (leaves x)
  (cond ((leaf? x)
         (list (leaf->label x)))
        ((node? x)
         (concat (leaves (node->left x))
                 (leaves (node->right x))))))

; direct style same?
(define (same? a b)
  (if (and (node? a) (node? b))
      (and (same? (node->left a)  (node->left b))
           (same? (node->right a) (node->right b)))
      (equal? a b)))

; direct style samefringe
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
(define size 10000)

(define (test name expected left right)
  (display "running ") (display name) (newline)
  (let ((actual (sameFringe left right)))
    (if (eq? expected actual)
        (begin (display "*SUCCESS*") (newline))
        (begin (display "*FAILURE*") (newline))))) 

(test "same leaves"       #t   (Leaf 1)                                           (Leaf 1))
(test "different leaves"  #f   (Leaf 1)                                           (Leaf 2))
(test "same trees"        #t   (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))           (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))
(test "rightish/rightish" #t   (Node (Leaf 0) (Node (generateRightishTree size)   (Leaf 0))) (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
(test "leftish/leftish"   #t   (Node (Leaf 0) (Node (generateLeftishTree size)    (Leaf 0))) (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "rightish/leftish"  #t   (Node (Leaf 0) (Node (generateRightishTree size)   (Leaf 0))) (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
(test "leftish/rightish"  #t   (Node (Leaf 0) (Node (generateLeftishTree size)    (Leaf 0))) (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
