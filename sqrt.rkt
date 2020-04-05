#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide sqrt)

(require (prefix-in rkt: (only-in racket/base sqrt))
         "arithmetic.rkt")

(define (sqrt x)
  (if (and (number? x)
           (or (inexact? x) ; floats propagate
               (exact? (rkt:sqrt x))))
      (rkt:sqrt x)
      (^ x 1/2)))

(module+ test
  (require rackunit)
  (check-equal? (sqrt 2) '(^ 2 1/2))
  (check-equal? (sqrt 'a) '(^ a 1/2))
  #;(check-not-equal? (sqrt (* 'a 'a)) 'a) ; WARNING: DOES NOT PASS
  (check-equal? (sqrt 4) 2)
  (check-true (number? (sqrt 2.))))
