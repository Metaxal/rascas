#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide sqrt)

(require (prefix-in rkt: (only-in racket/base sqrt))
         "arithmetic.rkt")

(define (sqrt x)
  (if (and (number? x)
           (exact? (rkt:sqrt x)))
      (rkt:sqrt x)
      (^ x 1/2)))
