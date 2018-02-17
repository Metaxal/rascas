#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide numerator)

(require (prefix-in rkt: (only-in racket/base numerator))
         racket/match
         "arithmetic.rkt")

  (define (numerator u)
    (match u
      [(? number?) (rkt:numerator u)]
      [`(^ ,x ,y)
        (if (and (number? y)
                 (negative? y))
            1
            u)]
      [`(* ,v . ,rest)
        (* (numerator v)
           (numerator (/ u v)))]
      [else u]))
