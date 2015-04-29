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

; leaves
(define (leaves x)
  (cond ((leaf? x)
         (list (leaf->label x)))
        ((node? x)
         (append (leaves (node->left x))
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
(sameFringe  (Leaf 1)                                  (Leaf 1))

(sameFringe  (Leaf 1)                                  (Leaf 2))

(sameFringe  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))  (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(sameFringe  (generateRightishTree 10000)              (generateRightishTree 10000))

(sameFringe  (generateLeftishTree 10000)               (generateLeftishTree 10000))

(sameFringe  (generateRightishTree 10000)              (generateLeftishTree 10000))

(sameFringe  (generateLeftishTree 10000)               (generateRightishTree 10000))
