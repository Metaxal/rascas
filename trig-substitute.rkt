#lang racket/base

(provide trig-substitute)

(require racket/match
         "arithmetic.rkt"
         "sin.rkt"
         "cos.rkt")

(define (trig-substitute u)
  (if (or (number? u)
          (symbol? u))
      u
      (let ((v (map trig-substitute u)))
        (match v
          [`(tan ,x) (/ (sin x) (cos x))]
          [`(cot ,x) (/ (cos x) (sin x))]
          [`(sec ,x) (/ 1 (cos x))]
          [`(csc ,x) (/ 1 (sin x))]
          [else v]))))
