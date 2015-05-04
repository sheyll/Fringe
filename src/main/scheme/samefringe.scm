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
(define (concat l1 l2)
  (if(null? l1)
     l2
     (cons (car l1) (concat (cdr l1) l2))))

; leaves
(define (leaves x)
  (cond ((leaf? x)
         (list (leaf->label x)))
        ((node? x)
         (concat (leaves (node->left x))
                 (leaves (node->right x))))))

; samefringe
(define (sameFringe a b)
  (equal? (leaves a)
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

(sameFringe  (Leaf 1)                                  (Leaf 1))

(sameFringe  (Leaf 1)                                  (Leaf 2))

(sameFringe  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))  (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(sameFringe  (generateRightishTree size)              (generateRightishTree size))

(sameFringe  (generateLeftishTree size)               (generateLeftishTree size))

(sameFringe  (generateRightishTree size)              (generateLeftishTree size))

(sameFringe  (generateLeftishTree size)               (generateRightishTree size))
