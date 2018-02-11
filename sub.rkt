#lang racket/base

(require racket/match
         "sum-product-power.rkt") ; + * ^

(provide - simplify-difference)

(define (simplify-difference u)
  (match u
    [`(- ,x)    (* -1 x)]
    [`(- ,x ,y) (+ x (* -1 y))]))

(define (- . elts)
  (simplify-difference `(- ,@elts)))

