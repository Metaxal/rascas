#lang racket/base

(provide ! simplify-factorial)

(require (prefix-in rkt: (only-in racket/base - *))
         (only-in math/number-theory factorial)
         racket/match)

(define (simplify-factorial u)
  (match u
    [`(! ,(? number? n)) (factorial n)]
    [`(! ,n) u]))

(define (! n)
  (simplify-factorial `(! ,n)))

