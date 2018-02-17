#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide denominator)

(require (rename-in (only-in racket/base denominator) (denominator rkt:denominator))
         racket/match
         "arithmetic.rkt")

(define (denominator u)
  (match u
    [(? number?) (rkt:denominator u)]
    [`(^ ,x ,y)
      (if (and (number? y)
               (negative? y))
          (^ u -1)
          1)]
    [`(* ,v . ,rest)
      (* (denominator v)
         (denominator (/ u v)))]
    [else 1]))
