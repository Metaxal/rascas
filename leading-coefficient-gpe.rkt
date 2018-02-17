#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide leading-coefficient-gpe)

(require "degree-gpe.rkt"
         "coefficient-gpe.rkt")

(define (leading-coefficient-gpe u x)
  (coefficient-gpe u x (degree-gpe u (list x))))
