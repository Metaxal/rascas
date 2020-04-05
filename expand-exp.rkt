#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide expand-exp)

(require "arithmetic.rkt"
         "exp.rkt"
         "misc.rkt"
         racket/match)

(define (expand-exp-rules u)
  (match u
    [`(+ ,v)
     (expand-exp-rules v)]
    [`(+ ,v . ,w)
     (* (expand-exp-rules v)
        (expand-exp-rules (cons '+ w)))]
    [`(* ,v)
     (expand-exp-rules v)]
    [`(* ,v . ,w)
     #:when (integer? v)
     (^ (expand-exp-rules (cons '* w)) v)]
    [else
     (exp u)]))

(define (expand-exp u)
  (if (list? u)
    (match (map expand-exp u)
      [`(exp ,v)
       (expand-exp-rules v)]
      [v v])
    u))
