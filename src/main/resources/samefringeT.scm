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

; continuations
(define *ticks* 0)
(define *answer* '())

(define (apply-cont k val)
  (lambda ()
    (set! *ticks* (+ *ticks* 1))
    (k val)))

(define (bounce thunk)
  (if (procedure? thunk)
      (bounce (thunk))))

; CPS style append
(define (concatK l1 l2 k)
  (if (null? l1)
      (apply-cont k l2)
      (concatK (cdr l1) l2 (lambda (l3)
                             (apply-cont k (cons (car l1) l3))))))

; CPS style leaves
(define (leavesK x k)
  (cond ((leaf? x)
         (let ((label (leaf->label x)))
           (apply-cont k (list label))))
        ((node? x)
         (leavesK (node->left x) (lambda (l)
                                   (leavesK (node->right x) (lambda (r)
                                                              (concatK l r k))))))))

; CPS style equal?
(define (equal?K a b k)
  (if (and (node? a) (node? b))
      (equal?K (node->left a) (node->left b) (lambda (e?)
                                               (if e?
                                                   (equal?K (node->right a) (node->right b) k)
                                                   (apply-cont k #f))))
      (apply-cont k (equal? a b))))

; CPS style samefringe
(define (sameFringeK a b k)
  (leavesK a (lambda (l1)
               (leavesK b (lambda (l2)
                            (equal?K l1 l1 k))))))

; direct style samefringe
(define (sameFringe a b)
  (set! *ticks* 0)
  (bounce
   (sameFringeK a b (lambda (answer)
                      (set! *answer* answer)
                      (display "ticks: ") (display *ticks*) (newline)
                      '())))
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

(sameFringe  (Leaf 1)                                  (Leaf 1))

(sameFringe  (Leaf 1)                                  (Leaf 2))

(sameFringe  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))  (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

(sameFringe  (generateRightishTree size)              (generateRightishTree size))

(sameFringe  (generateLeftishTree size)               (generateLeftishTree size))

(sameFringe  (generateRightishTree size)              (generateLeftishTree size))

(sameFringe  (generateLeftishTree size)               (generateRightishTree size))
