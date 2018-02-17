#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide rational-expand)

(require "arithmetic.rkt"
         "algebraic-expand.rkt"
         "numerator.rkt"
         "denominator.rkt"
         "rational-gre.rkt"
         "rationalize-expression.rkt")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rational-expand u)
  (let ((f (algebraic-expand (numerator   u)))
        (g (algebraic-expand (denominator u))))
    (if (equal? g 0)
        #f
        (let ((h (rationalize-expression (/ f g))))
          (if (equal? h u)
              u
              (rational-expand h))))))
