#lang racket
(require "Tree.scm")
(provide sameFringe implStyle)

(define implStyle "trampolined style")

;; continuations
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
    (k val)))

(define (bounce thunk)
  (set! *ticks* (+ *ticks* 1))
  (if (procedure? thunk)
      (bounce (thunk))
      thunk))

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
