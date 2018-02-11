#lang racket/base

(provide log)

(require "misc.rkt"
         (prefix-in rkt: (only-in racket/base log)))

(define log
  (case-lambda
    [(x)
     (cond [(number? x)
            (rkt:log x)]
           [(exp? x) (list-ref x 1)]
           [else `(log ,x)])]
    [(x y)
     (cond [(and (number? x) (number? y))
            (rkt:log x y)]
           [else `(log ,x ,y)])]))

