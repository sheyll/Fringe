#lang racket
(require "Tree.scm")
(provide sameFringe implStyle)

(define implStyle "direct style")

; concat
; direct style
(define (concat l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat (cdr l1) l2))))

; leaves
; direct style
(define (leaves x)
  (cond ((leaf? x)
         (cons (leaf->label x) '()))
        ((node? x)
         (concat (leaves (node->left x))
                 (leaves (node->right x))))))

; same?
; direct style
(define (same? a b)
  (if (null? a)
      (null? b)
      (if (equal? (car a) (car b))
          (same? (cdr a) (cdr b))
          #f)))

; sameFringe
; direct style
(define (sameFringe a b)
  (same? (leaves a)
         (leaves b)))
