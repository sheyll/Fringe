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

; CPS style append
(define (appendK l1 l2 k)
  (if (null? l1)
      (k l2)
      (let ((l1h (car l1))
            (l1t (cdr l1)))
        (appendK l1t l2 (lambda (l3)
                          (k (cons l1h l3)))))))

; CPS style leaves
(define (leavesK x k)
  (cond ((leaf? x)
         (let ((label (leaf->label x)))
           (k (list label))))
        ((node? x)
         (let ((left (node->left x))
               (right (node->right x)))
           (leavesK left (lambda (l)
                           (leavesK right (lambda (r)
                                            (appendK l r k)))))))))

; CPS style equal?
(define (equal?K a b k) ;; FIXME convert to CPS
  (equal? a b))

; CPS style samefringe
(define (sameFringeK a b k) ;; FIXME convert to CPS
  (equal?K (leavesK a k)
           (leavesK b k)
           k))

; direct style samefringe
(define (sameFringe a b)
  (sameFringeK a b id))

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

(sameFringe  (Leaf 1)                                  (Leaf 1))

(sameFringe  (Leaf 1)                                  (Leaf 2))

(sameFringe  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))  (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(sameFringe  (generateRightishTree size)              (generateRightishTree size))

(sameFringe  (generateLeftishTree size)               (generateLeftishTree size))

(sameFringe  (generateRightishTree size)              (generateLeftishTree size))

(sameFringe  (generateLeftishTree size)               (generateRightishTree size))
