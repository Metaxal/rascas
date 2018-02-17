#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide degree-gpe) 

(require (only-in srfi/1 every)
         "misc.rkt"
         "contains.rkt"
         )

(define (degree-monomial-gpe u v)
  (cond ( (every (is-free? u) v) 0 )
        ( (member u v) 1 )
        ( (and (power? u)
               (let ((n (list-ref u 2)))
                 (integer? n)
                 (> n 1)))

          (list-ref u 2) )
        ( (product? u)
          (apply +
                 (map
                  (lambda (elt)
                    (degree-monomial-gpe elt v))
                  (cdr u))) )
        ( else 0 )))

(define (degree-gpe u v)
  (cond ( (sum? u)
          (apply max
                 (map
                  (lambda (elt)
                    (degree-monomial-gpe elt v))
                  (cdr u))) )
        ( else (degree-monomial-gpe u v) )))
