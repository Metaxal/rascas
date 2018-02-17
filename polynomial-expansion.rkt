#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide polynomial-expansion)

(require "arithmetic.rkt"
         "algebraic-expand.rkt"
         "polynomial-division.rkt")

(define (polynomial-expansion u v x t)
  (if (equal? u 0)
      0
      (let ((d (polynomial-division u v x)))
        (let ((q (list-ref d 0))
              (r (list-ref d 1)))
          (algebraic-expand (+ (* t (polynomial-expansion q v x t))
                               r))))))
