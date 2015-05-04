;; functions
(define (id x) x)

(define (compose f g) (lambda (x) (f (g x))))

; continuations
(define *depth* 0)
(define *max-depth* 0)
(define *ticks* 0)
(define *answer* '())

(define (build-cont f)
  (set! *depth* (+ *depth* 1))
  (set! *max-depth* (max *depth* *max-depth*))
  f)

(define (apply-cont k val)
  (lambda ()
    (set! *depth* (- *depth* 1))
    (set! *ticks* (+ *ticks* 1))
    (k val)))

(define (bounce thunk)
  (if (procedure? thunk)
      (bounce (thunk))))

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

; CPS style concat
(define (concatK l1 l2 k)
  (if (null? l1)
      (apply-cont k l2)
      (concatK (cdr l1) l2 (build-cont (lambda (l3)
                                         (apply-cont k (cons (car l1) l3)))))))

; CPS style leaves
(define (leavesK x k)
  (cond ((leaf? x)
         (apply-cont k (list (leaf->label x))))
        ((node? x)
         (leavesK (node->left x) (build-cont (lambda (l)
                                               (leavesK (node->right x) (build-cont (lambda (r)
                                                                                      (concatK l r k))))))))))

; CPS style same?
(define (same?K a b k)
  (if (and (node? a) (node? b))
      (same?K (node->left a) (node->left b) (build-cont (lambda (e?)
                                                          (if e?
                                                              (same?K (node->right a) (node->right b) k)
                                                              (apply-cont k #f)))))
      (apply-cont k (equal? a b))))

; CPS style samefringe
(define (sameFringeK a b k)
  (leavesK a (build-cont (lambda (l1)
                           (leavesK b (build-cont (lambda (l2)
                                                    (same?K l1 l2 k))))))))

; direct style samefringe
(define (sameFringe a b)
  (set! *depth* 0)
  (set! *max-depth* 0)
  (set! *ticks* 0)
  (bounce
   (sameFringeK a b (build-cont (lambda (answer)
                                  (set! *answer* answer)
                                  (display "depth:     ") (display *depth*) (newline)
                                  (display "max-depth: ") (display *max-depth*) (newline)
                                  (display "ticks:     ") (display *ticks*) (newline)
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
