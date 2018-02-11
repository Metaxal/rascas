#lang racket/base

(provide expand-exp)

(require "arithmetic.rkt"
         "exp.rkt"
         "misc.rkt")

(define (expand-exp-rules A)
  (cond ( (sum? A)
          (let ((f (list-ref A 1)))
            (* (expand-exp-rules f)
               (expand-exp-rules (- A f)))) )
        ( (product? A)
          (let ((f (list-ref A 1)))
            (if (integer? f)
                (^ (expand-exp-rules (/ A f)) f)
                (exp A))) )
        (else (exp A))))

(define (expand-exp u)
  (if (or (number? u) (symbol? u))
      u
      (let ((v (map expand-exp u)))
        (if (exp? v)
            (expand-exp-rules (list-ref v 1))
            v))))
