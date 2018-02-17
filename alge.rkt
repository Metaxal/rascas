#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide alge)

(require "automatic-simplify.rkt"
         "infix-parser.rkt")

(define (alge val)
  (automatic-simplify 
   (if (string? val)
       (alg val)
       val)))
