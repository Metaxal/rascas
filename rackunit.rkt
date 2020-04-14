#lang racket/base

(require rackunit)
(provide (all-from-out rackunit)
         check-values)

;; Useful to check thunks that return multiple values.
;; (don't forget to use a thunk)
(define-simple-check (check-values cmp thunk lst)
  (cmp (call-with-values thunk list) lst))


