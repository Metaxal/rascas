#lang racket/base

(provide alge)

(require "automatic-simplify.rkt"
         "infix-parser.rkt")

(define (alge val)
  (automatic-simplify 
   (if (string? val)
       (alg val)
       val)))
