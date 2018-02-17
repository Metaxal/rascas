#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide monomial-gpe?
        monomial-gpe-in?
        is-monomial-gpe?)

(require (only-in srfi/1 every)
        "misc.rkt"
        "contains.rkt")

(define (monomial-gpe? u v)
  (or (every (is-free? u) v) ;; GME-1
      (member u v) ;; GME-2
      (and (power? u) ;; GME-3
           (let ((n (list-ref u 2)))
             (integer? n)
             (> n 1))) 
      (and (product? u) ;; GME-4
           (every (monomial-gpe-in? v) (cdr u))))) 

(define (monomial-gpe-in? v)
  (lambda (u)
    (monomial-gpe? u v)))

(define (is-monomial-gpe? u)
  (lambda (v)
    (monomial-gpe? u v)))
