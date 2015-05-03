;; binary tree

; leaf
(define (Leaf a)
  a)

(define (leaf? x)
  (not (pair? x)))

(define (leaf->label x)
  x)

; node
(define (Node left right)
  (cons left right))

(define (node? x)
  (pair? x))

(define (node->left x)
  (car x))

(define (node->right x)
  (cdr x))

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

;; samefringe
(define (sameFringe a b)
  (equal? (leaves a)
          (leaves b)))

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

(sameFringe  (Leaf 1)                                  (Leaf 1))

(sameFringe  (Leaf 1)                                  (Leaf 2))

(sameFringe  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))  (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(sameFringe  (generateRightishTree size)              (generateRightishTree size))

(sameFringe  (generateLeftishTree size)               (generateLeftishTree size))

(sameFringe  (generateRightishTree size)              (generateLeftishTree size))

(sameFringe  (generateLeftishTree size)               (generateRightishTree size))
