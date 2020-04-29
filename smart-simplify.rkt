#lang racket/base

(require "color-printf.rkt"
         "misc.rkt"
         "algorithmic.rkt"
         "automatic-simplify.rkt"
         "factor.rkt"
         "distribute.rkt"
         racket/sandbox
         racket/dict
         racket/list)

(provide smart-simplify)

(define trans-dict
  (ids->assoc contract-let*
              contract-let*/ascovars
              distribute-product
              factor-product
              automatic-simplify
              expand-let*))

;; Tries hard to simplify u with various strategies and returns the transformed tree
;; with the least tree-cost among the various sequences that have been tried.
;; Still work in progress.
;; Uses a kind of iterative doubling Levin tree search to spread the search time evenly
;; among the different branches and to ensure that no single branch is taking too much time
;; (the attempted transformation is halted early if it goes overtime).
;; TODO: Check for trigonometric functions and apply trig-simplify. Same for contract/expand of exp?
;; TODO: timeout to return the best result if take too much time!
;; The default time limit is merely the tree size. This is entirely heuristic!
(define (smart-simplify u
                        #:tree-cost [tree-cost tree-size]
                        #:time-limit [time-limit (tree-size u)] ; in ms
                        #:first-time-limit [first-time-limit time-limit]
                        #:memory-limit [memory-limit #f]
                        #:info? [info? #f])

  (define best-u u)
  (define best-tree-cost (tree-cost u))
  (define best-trans-seq '())

  (define pre-DFS (current-milliseconds))
  (let DFS ([cost-limit first-time-limit])
    (define pre-loop (current-milliseconds))
    (let loop ([u u] [depth 0] [cost 0] [inv-proba 1] [trans-seq '()])
      (define mprintf
        (and info?
             (λ (fmt #:color [color #f] . v)
               (apply
                printf
                #:color color
                (string-append "smart-simplify: " (make-string depth #\.) fmt)
                v))))
      #;(newline)
      #;(read-line)
      #;(debug cost-limit depth cost inv-proba trans-seq)
      (define local-trans (shuffle trans-dict)) ; shuffle to help our luck
      (define n-children (length local-trans))
      (define new-inv-proba (* inv-proba n-children))
      (define tmax (/ (- cost-limit cost 0.) new-inv-proba))
      (when (> tmax 1)
        (for ([(name trans) (in-dict local-trans)])
          (when info?
            (mprintf "~a tmax: ~a " name tmax))
          (define pre (current-milliseconds))
          (define-values (new-u new-s)
            (with-handlers ([exn:fail:resource? (λ (e)
                                                  (when info? (printf "killed "))
                                                  (values #f +inf.0))])
              ;; warning: killing thread that's overtime takes about 20ms?
              (with-limits (* 0.001 tmax) memory-limit
                (define new-u (trans u))
                (values new-u (tree-cost new-u)))))
          (define post (current-milliseconds))
          (define tdiff (max 1 (- post pre)))
          (define new-trans-seq (cons name trans-seq))
          #;(debug name tmax new-s tdiff)
          (when info?
            (unless (= +inf.0 new-s) (printf "size: ~a " new-s))
            (printf "cpu: ~a\n" tdiff))
          (when (< new-s best-tree-cost)
            (when info?
              (mprintf "new best size: ~a\n" new-s #:color 'green))
            (set! best-u new-u)
            (set! best-tree-cost new-s)
            (set! best-trans-seq (reverse new-trans-seq)))
          (loop new-u (+ depth 1) (+ cost (* tdiff new-inv-proba)) new-inv-proba new-trans-seq))))
    (define post-loop (current-milliseconds))
    (define remaining-time (- time-limit (- post-loop pre-DFS)))
    (define next-cost-limit
      (min (* 4 cost-limit) remaining-time))
    #;(newline)
    #;(debug remaining-time next-cost-limit)
    (when (> next-cost-limit 1)
      (when info?
        (define loop-time (- post-loop pre-loop))
        (debug loop-time remaining-time next-cost-limit))
      (DFS next-cost-limit)))
  (when info?
    (debug best-trans-seq best-tree-cost))
  best-u)

;; Example:
(module+ main #;drracket
  (require "color-printf.rkt"
           "derivative.rkt"
           "arithmetic.rkt"
           "special-functions.rkt")
  (current-use-colors? #f)
  
  (define df (derivative '(* (+ 1 x) (+ 2 x) (gamma (+ 3 x)) (exp (+ 4 x))
                             (sqr (+ 5 x)) (^ (log (+ 6 x)) -1))
                         'x))
  df
  (displayln (list 'tree-size: (tree-size df)))
  (displayln "Using simplify:")
  (define df-simple (time (smart-simplify df #:info? #t)))
  df-simple
  (displayln (list 'tree-size: (tree-size df-simple)))
  (displayln "Taking the second derivative:")
  (define ddf (derivative df-simple 'x))
  (displayln (list 'ddf-tree-size: (tree-size ddf)))
  (displayln (list 'ddf-simple-tree-size: (tree-size (time (smart-simplify ddf #:info? #t)))))
  )
