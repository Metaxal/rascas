#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide expand-power)

(require "misc.rkt"
         "arithmetic.rkt"
         "factorial.rkt"
         "expand-product.rkt")

(define (expand-power u n)
  (if (sum? u)
      (let ((f (list-ref u 1)))
        (let ( (r (- u f)) )
          (let loop ( (s 0)
                      (k 0) )
            (if (> k n)
                s
                (let ((c (/ (! n)
                            (* (! k)
                               (! (- n k))))))
                  (loop (+ s 
                           (expand-product (* c (^ f (- n k)))
                                           (expand-power r k)))
                        (+ k 1)))))))
      (^ u n)))
