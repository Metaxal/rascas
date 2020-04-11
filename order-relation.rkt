#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/match
         "misc.rkt")

(provide term
         const
          
         order-relation)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (term u)

    (match u
      [(? number?) #f]
      [`(* ,(and u1 (? number?)) . ,u-rest)
       `(* ,@u-rest)]
      [`(* . ,u-elts) u]
      [else `(* ,u)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (const u)
    (match u
      [(? number?) #f]
      [`(* ,(and u1 (? number?)) . ,u-rest)
       u1]
      [`(* . ,u-elts) 1]
      [else 1]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (O-3 u-elts v-elts)
    (cond ((null? u-elts) #t)
          ((null? v-elts) #f)
          (else
           (let ((u (car u-elts))
                 (v (car v-elts)))
             (if (not (equal? u v))
                 (order-relation u v)
                 (O-3 (cdr u-elts)
                      (cdr v-elts)))))))

  (define (order-relation u v [already-exchanged? #f])

    (cond ((and (number? u)
                (number? v)) ;; O-1
           (< u v))

          ((and (symbol? u)
                (symbol? v)) ;; O-2

           (symbol<? u v))

          ((or (and (product? u)
                    (product? v))
               (and (sum?     u)
                    (sum?     v))) ;; O-3
           (O-3 (reverse (cdr u))
                (reverse (cdr v))))

          ((and (power? u)
                (power? v)) ;; O-4

           (if (equal? (base u)
                       (base v))

               (order-relation (exponent u)
                               (exponent v))

               (order-relation (base u)
                               (base v))))

          ((and (factorial? u)
                (factorial? v)) ;; O-5

           (order-relation (list-ref u 1)
                           (list-ref v 1)))

          ((and (function? u)
                (function? v)) ;; O-6

           (if (equal? (car u)
                       (car v))

               (O-3 (cdr u)
                    (cdr v))

               (order-relation (car u)
                               (car v))))

          ((and (number? u)
                (not (number? v))) ;; O-7
           #t)

          ((and (product? u)
                (or (power? v)
                    (sum? v)
                    (factorial? v)
                    (function? v)
                    (symbol? v))) ;; O-8
           (order-relation u `(* ,v)))

          ((and (power? u)
                (or (sum? v)
                    (factorial? v)
                    (function? v)
                    (symbol? v))) ;; O-9

           (order-relation u `(^ ,v 1)))

          ((and (sum? u)
                (or (factorial? v)
                    (function? v)
                    (symbol? v))) ;; O-10

           (order-relation u `(+ ,v)))

          ((and (factorial? u)
                (or (function? v)
                    (symbol? v))) ;; O-11
           
           (if (equal? (list-ref u 1) v)
               #f
               (order-relation u `(! ,v))))

          ((and (function? u)
                (symbol? v)) ;; O-12
           (if (equal? (car u) v)
               #f
               (order-relation (car u) v)))

          [already-exchanged?
           (error "Cannot find an order relation for " (list u v))]
          (else ;; O-13
           (not (order-relation v u #t)))))
