#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide alg-mult-inverse
         alg-divide
         alg-coeff-simp
         alg-polynomial-division
         alg-quotient
         alg-remainder)

(require "arithmetic.rkt"
         "misc.rkt"
         "degree-gpe.rkt"
         "leading-coefficient-gpe.rkt"
         "collect-terms.rkt"
         "algebraic-expand.rkt"
         "polynomial-division.rkt"
         "extended-euclidean-algorithm.rkt")

(define (alg-mult-inverse v p a)
  (list-ref (extended-euclidean-algorithm v p a) 1))

(define (alg-divide u v p a)
  (remainder (algebraic-expand
              (* u (alg-mult-inverse v p a)))
             p a))

(define (alg-coeff-simp u x p a)
  (collect-terms (remainder u p a) (list x)))

(define (alg-polynomial-division u v x p a)
  (let ((q 0)
        (r u)
        (m (degree-gpe u '(x)))
        (n (degree-gpe v '(x)))
        (lcv (leading-coefficient-gpe v x))
        (lcr #f)
        (s #f))
    (while (>= m n)
      (set! lcr (leading-coefficient-gpe r x))
      (set! s (alg-divide lcr lcv p a))
      (set! q (+ q (* s (^ x (- m n)))))
      (set! r (algebraic-expand
               (- (- r (* lcr (^ x m)))
                  (* (- v (* lcv (^ x n)))
                     s
                     (^ x (- m n))))))
      (set! r (alg-coeff-simp r x p a))
      (set! m (degree-gpe r '(x))))
    (list q r)))

(define (alg-quotient u v x p a)
  (list-ref (alg-polynomial-division u v x p a) 0))

(define (alg-remainder u v x p a)
  (list-ref (alg-polynomial-division u v x p a) 1))
