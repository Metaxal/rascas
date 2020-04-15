#lang racket/base

(require "misc.rkt"
         "letstar.rkt")
(provide (all-from-out "letstar.rkt")
         _list)

(module+ test (require "rackunit.rkt"))

;; Just a normal list.
;; Not really 'arithmetic', but too simple to put in a separate file.
(define (_list . args)
  (cons 'list args))
(register-function 'list _list)

(module+ test
  (check-equal? (_list 'a 'b 'c)
                '(list a b c)))
