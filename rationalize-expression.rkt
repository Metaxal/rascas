#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide rationalize-expression)

(require "misc.rkt"
         "arithmetic.rkt"
         "numerator.rkt"
         "denominator.rkt")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rationalize-sum u v)
  (let ((m (numerator u))
        (r (denominator u))
        (n (numerator v))
        (s (denominator v)))
    (if (and (equal? r 1)
             (equal? s 1))
        (+ u v)
        (/ (rationalize-sum (* m s)
                            (* n r))
           (* r s)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rationalize-expression u)
  (cond ( (power? u)
          (^ (rationalize-expression (list-ref u 1))
             (list-ref u 2)) )
        ( (product? u)
          (let ((f (list-ref u 1)))
            (* (rationalize-expression f)
               (rationalize-expression (/ u f)))) )
        ( (sum? u)
          (let ((f (list-ref u 1)))
            (let ((g (rationalize-expression f))
                  (r (rationalize-expression (- u f))))
              (rationalize-sum g r))) )
        ( else u )))

