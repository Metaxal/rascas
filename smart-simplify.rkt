#lang racket/base

(require "misc.rkt"
         "automatic-simplify.rkt"
         "factor.rkt"
         "distribute.rkt")

(provide smart-simplify)

;; Tries hard to simplify u with various strategies.
;; Still work in progress.
;; TODO: Check for trigonometric functions and apply trig-simplify. Same for contract/expand of exp?
(define (smart-simplify u)
  (for/fold ([best u]
             [best-size (tree-size u)]
             #:result best)
            ([ltrans (in-list
                      ;; Other transformations may be added here later
                      (list (list factor-product)
                            (list automatic-simplify distribute-product factor-product)
                            (list automatic-simplify factor-product)
                            (list distribute-product factor-product)
                            ))])
    ;; Follows a list of transformations starting from u,
    ;; and keeps the best one along the path.
    (for/fold ([best best]
               [best-size best-size]
               [u u]
               #:result (values best best-size))
              ([trans (in-list ltrans)])
      (define new-u (trans u))
      (define s (tree-size new-u))
      (if (< s best-size)
        (values new-u s new-u)
        (values best best-size new-u)))))

;; Example:
(module+ drracket
  (require "derivative.rkt"
           "arithmetic.rkt"
           "misc.rkt")
  (define df (derivative '(* (+ 1 x) (+ 2 x) (gamma (+ 3 x)) (exp (+ 4 x))
                             (sqr (+ 5 x)) (^ (log (+ 6 x)) -1))
                         'x))
  df
  (displayln (list 'tree-size: (tree-size df)))
  (displayln "Using simplify:")
  (define df-simple (smart-simplify df))
  df-simple
  (displayln (list 'tree-size: (tree-size df-simple)))
  (displayln "Taking the second derivative:")
  (define ddf (derivative df-simple 'x))
  (displayln (list 'ddf-tree-size: (tree-size ddf)))
  (displayln (list 'ddf-simple-tree-size: (tree-size (smart-simplify ddf))))
  )
