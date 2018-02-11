#lang racket/base

(provide leading-coefficient-gpe)

(require "degree-gpe.rkt"
         "coefficient-gpe.rkt")

(define (leading-coefficient-gpe u x)
  (coefficient-gpe u x (degree-gpe u (list x))))
