#lang racket/base

(provide sqrt)

(require (prefix-in rkt: (only-in racket/base sqrt))
         "arithmetic.rkt")

(define (sqrt x)
  (if (and (number? x)
           (exact? (rkt:sqrt x)))
      (rkt:sqrt x)
      (^ x 1/2)))
