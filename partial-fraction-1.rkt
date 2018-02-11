#lang racket/base

(provide partial-fraction-1)

(require "arithmetic.rkt"
         "algebraic-expand.rkt"
         "polynomial-division.rkt"
         "extended-euclidean-algorithm.rkt")

(define (partial-fraction-1 u v1 v2 x)
  (let ((s (extended-euclidean-algorithm v1 v2 x)))
    (let ((A (list-ref s 1))
          (B (list-ref s 2)))
      (list (remainder (algebraic-expand (* B u)) v1 x)
            (remainder (algebraic-expand (* A u)) v2 x)))))
