#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide alg-monic
         alg-polynomial-gcd)

(require "leading-coefficient-gpe.rkt"
         "alg-polynomial-division.rkt")

(define (alg-monic u x p a)
  (alg-divide u (leading-coefficient-gpe u x) p a))

(define (alg-polynomial-gcd u v x p a)
  (let loop ((u u) (v v))
    (if (equal? v 0)
        (alg-monic u x p a)
        (loop v (alg-remainder u v x p a)))))
