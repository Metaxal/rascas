#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require (prefix-in rkt: (only-in racket/base exp)))

(provide exp)

(define (exp u)
  (if (number? u)
      (rkt:exp u)
      `(exp ,u)))
