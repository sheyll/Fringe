#lang racket
(require "Tree.scm")
(provide sameFringe implStyle)

(define implStyle "continuation passing style")

;; continuations
(define (build-cont f)
  f)

(define (apply-cont k val)
  (k val))

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

; sameFringe
; CPS style
(define (sameFringeK a b k)
  (leavesK a (build-cont (lambda (l1)
                           (leavesK b (build-cont (lambda (l2)
                                                    (same?K l1 l2 k))))))))

; sameFringe
; direct style
(define (sameFringe a b)
  (sameFringeK a b (build-cont (lambda (answer)
                                 answer))))
