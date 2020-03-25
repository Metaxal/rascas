#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require (prefix-in rkt: (only-in racket/base exp)))

(provide exp)

(define (exp u)
  (if (and (number? u)
           (or (inexact? u)
               (exact? (rkt:exp u))))
      (rkt:exp u)
      `(exp ,u)))
