#lang racket/base

(provide rational-gre?)

(require "polynomial-gpe.rkt"
         "numerator.rkt"
         "denominator.rkt")

(define (rational-gre? u v)
  (and (polynomial-gpe? (numerator   u) v)
       (polynomial-gpe? (denominator u) v)))
