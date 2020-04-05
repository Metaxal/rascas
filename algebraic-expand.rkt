#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide algebraic-expand)

(require "misc.rkt"
         "arithmetic.rkt"
         "automatic-simplify.rkt")

(define (algebraic-expand u)
  (cond ( (sum? u)
          (apply + (map algebraic-expand (cdr u))) )
        ( (product? u)
          (let ((v (list-ref u 1)))
            (expand-product (algebraic-expand v)
                            (algebraic-expand (/ u v)))) )
        ( (power? u)
          (let ((base     (list-ref u 1))
                (exponent (list-ref u 2)))
            (if (and (integer? exponent)
                     (>= exponent 2))
                (expand-power (algebraic-expand base)
                              exponent)
                u)) )
        ( (list? u)
          (automatic-simplify
           (map algebraic-expand u)) )
        ( else u )))
