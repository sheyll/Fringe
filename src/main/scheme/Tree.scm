#lang racket

;; binary tree

(provide Leaf leaf? leaf->label)
(provide Node node? node->left node->right)

; leaf
(define Leaf identity)

(define leaf? (compose not pair?))

(define leaf->label identity)

; node
(define Node cons)

(define node? pair?)

(define node->left car)

(define node->right cdr)
