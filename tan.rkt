#lang racket/base

(provide tan)

(require "arithmetic.rkt"
         racket/match)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-tan u)
  (match u
    [`(tan (* ,(? (λ(n)(and (number? n)
                            (negative? n)))
                  n)
              . ,elts))
      (- (tan (apply * (append (list -1 n) elts))))]
    [else u]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tan x)
  (simplify-tan `(tan ,x)))
