#lang racket/base

;; Do NOT remove the 'unused' dependencies, they may be used by eval.
(require racket/math
         math/special-functions
         (only-in "arithmetic.rkt" dirac)
         (only-in "algorithmic.rkt" >0 >=0))

(provide compile-tree)

;;;; `compile-tree` compiles the s-expression tree to a full racket procedure,
;;;; thus likely making it more efficient.
;;;; This may not work if other functions have been registered by the user.
;;;; In this case, use `tree->procedure` from 'substitute.rkt' instead.

(define ^ expt)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; In case there are many arguments (say millions), would it be faster to use a vector
;; instead of passing them raw?
(define (compile-tree t . vars)
  (unless (andmap symbol? vars)
    (raise-argument-error 'compile-tree "(listof symbol?)" vars))
  (define t2
    `(λ ,vars ,t))
  (eval t2 ns))
