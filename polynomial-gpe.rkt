#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide polynomial-gpe?
        polynomial-gpe-in?
        is-polynomial-gpe?)

(require (only-in srfi/1 every)
        "misc.rkt"
        "monomial-gpe.rkt")

(define (polynomial-gpe? u v)
  (or (monomial-gpe? u v)
      (and (sum? u)
           (every (monomial-gpe-in? v) (cdr u)))))

(define (polynomial-gpe-in? v)
  (lambda (u)
    (polynomial-gpe? u v)))

(define (is-polynomial-gpe? u)
  (lambda (v)
    (polynomial-gpe? u v)))
