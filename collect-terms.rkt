#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide collect-terms)

(require (only-in srfi/1 iota)
         "misc.rkt"
         "arithmetic.rkt"
         "coeff-var-monomial.rkt")

(define (collect-terms u S)
  (cond ( (not (sum? u)) u )
        ( (member u S) u )
        ( else
          (let ((N 0)
                (T (make-hasheq)))
            (for-each
             (lambda (i)
               (let ((f (coeff-var-monomial (list-ref u i) S)))
                 (let ((j 1)
                       (combined #f))
                   (while (and (not combined)
                               (<= j N))
                          (when (equal? (list-ref f 1)
                                        (list-ref (hash-ref T j '(#f #f)) 1))
                            (hash-set! T
                                       j
                                       (list (+ (list-ref f 0)
                                                (list-ref (hash-ref T j #f)
                                                          0))
                                             (list-ref f 1)))
                            (set! combined #t))
                          (set! j (+ j 1)))
                   (when (not combined)
                     (hash-set! T (+ N 1) f)
                     (set! N (+ N 1))))))
             (cdr (iota (length u))))
            (apply +
                   (map 
                    (lambda (val)
                      (apply * val))
                    (hash-values T)))))))
