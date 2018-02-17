#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide polynomial-gcd)

(require "arithmetic.rkt"
         "algebraic-expand.rkt"
         "leading-coefficient-gpe.rkt"
         "polynomial-division.rkt")

(define (polynomial-gcd u v x)
  (if (and (equal? u 0)
           (equal? v 0))
      0
      (let loop ((u u) (v v))
        (if (equal? v 0)
            (algebraic-expand
             (* (/ 1 (leading-coefficient-gpe u x))
                u))
            (loop v (remainder u v x))))))
