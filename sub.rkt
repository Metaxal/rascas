#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/match
         "sum-product-power.rkt") ; + * ^

(provide - simplify-difference)

(define (simplify-difference u)
  (match u
    [`(- ,x)    (* -1 x)]
    [`(- ,x ,y) (+ x (* -1 y))]))

(define (- . elts)
  (simplify-difference `(- ,@elts)))

