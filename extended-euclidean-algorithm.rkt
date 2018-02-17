#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide extended-euclidean-algorithm)

(require "arithmetic.rkt"
         "leading-coefficient-gpe.rkt"
         "algebraic-expand.rkt"
         "polynomial-division.rkt")

(define (extended-euclidean-algorithm u v x)
  (if (and (equal? u 0)
           (equal? v 0))
      (list 0 0 0)
      (let loop ((u u)
                 (v v)
                 (App 1)
                 (Ap  0)
                 (A   #f)
                 (Bpp 0)
                 (Bp  1)
                 (B   #f))
        (if (equal? v 0)
            (let ((c (leading-coefficient-gpe u x)))
              (list (algebraic-expand (/ u c))
                    (algebraic-expand (/ App c))
                    (algebraic-expand (/ Bpp c))))
            (let ((q (quotient  u v x))
                  (r (remainder u v x)))
              (let ((A (- App (* q Ap)))
                    (B (- Bpp (* q Bp))))
                (loop v r Ap A A Bp B B)))))))