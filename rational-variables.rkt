#lang racket/base

(provide rational-variables)

(require (only-in srfi/1 lset-union)
         "variables.rkt"
         "numerator.rkt"
         "denominator.rkt"
         )

(define (rational-variables u)
  (lset-union equal?
              (variables (numerator   u))
              (variables (denominator u))))
