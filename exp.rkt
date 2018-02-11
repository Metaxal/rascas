#lang racket/base

(require (prefix-in rkt: (only-in racket/base exp)))

(provide exp)

(define (exp u)
  (if (number? u)
      (rkt:exp u)
      `(exp ,u)))
