#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide coeff-var-monomial)

(require "arithmetic.rkt"
         "contains.rkt")

(define (coeff-var-monomial u v)
  (let loop ( (coefficient-part u)
              (variables v) )
    (cond ( (null? variables)
            (let ((variable-part (/ u coefficient-part)))
              (list coefficient-part variable-part)) )
          ( (free? u (car variables))
            (loop coefficient-part
                  (cdr variables)) )
          ( else
            (loop (/ coefficient-part (car variables))
                  (cdr variables)) ))))
