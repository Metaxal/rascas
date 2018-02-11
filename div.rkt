#lang racket/base

(require racket/match
         "sum-product-power.rkt") ; + * ^

(provide / simplify-quotient)

(define (simplify-quotient u)
  (match u
    [`(/ ,x ,y)
     (* x (^ y -1))]))

(define (/ u v)
  (simplify-quotient `(/ ,u ,v)))

  