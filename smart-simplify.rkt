#lang racket/base

(require "color-printf.rkt"
         "misc.rkt"
         "algorithmic.rkt"
         "automatic-simplify.rkt"
         "factor.rkt"
         "distribute.rkt"
         "rich-return.rkt"
         racket/sandbox
         racket/dict
         racket/list
         racket/match)

(provide smart-simplify
         apply-trans-seq)

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
;; At each iteration, the loop uses 4x the previous time budget. The first budget is first-time-limit.
;; The previous best tree is also used.
;; This means that it's often more advantageous to do have time limit >> first-time-limit,
;; so as to benefit from quick and greedy improvements.
;; Notice: Some transformations use hashes, which can lead to non-deterministic results.
;; TODO: Check for trigonometric functions and apply trig-simplify. Same for contract/expand of exp?
;; TODO: timeout to return the best result if take too much time!
;; The default time limit is merely the tree size. This is entirely heuristic!
;; TODO: Favor applying more transformations to the best found so far.
;; TODO: make-tree-cost to give higher costs to some functions (like gamma). Even better:
;;   a benchmark file to calculate the speed of various functions and assign costs automatically.
(define (smart-simplify tree
                        #:return [return 'tree]
                        #:tree-cost [tree-cost tree-size]
                        #:time-limit [time-limit (tree-size tree)] ; in ms
                        #:first-time-limit [first-time-limit time-limit]
                        #:memory-limit [memory-limit #f]
                        #:info? [info? #f])

  (define best-tree tree)
  (define best-tree-cost (tree-cost tree))
  (define best-trans-seq '())
  (define best-seq-time 0) ; cumulative time taken by the sequence trans-seq

  (define pre-DFS (current-milliseconds))
  (let DFS ([cost-limit first-time-limit])
    (define pre-loop (current-milliseconds))
    (let loop ([tree best-tree]
               [seq-time best-seq-time] [depth 0] [cost 0] [inv-proba 1]
               [trans-seq best-trans-seq])
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
          (define-values (new-tree new-tree-cost)
            (with-handlers ([exn:fail:resource? (λ (e)
                                                  (when info? (printf "killed "))
                                                  (values #f +inf.0))])
              ;; warning: killing thread that's overtime takes about 20ms?
              (with-limits (* 0.001 tmax) memory-limit
                (define new-tree (trans tree))
                (values new-tree (tree-cost new-tree)))))
          (define post (current-milliseconds))
          (define tdiff (max 1 (- post pre)))
          (define new-trans-seq (cons name trans-seq))
          (define new-seq-time (+ seq-time tdiff))
          #;(debug name tmax new-s tdiff)
          (when info?
            (unless (= +inf.0 new-tree-cost) (printf "size: ~a " new-tree-cost))
            (printf "cpu: ~a\n" tdiff))
          (when (or (< new-tree-cost best-tree-cost)
                    (and (= new-tree-cost best-tree-cost)
                         (< new-seq-time best-seq-time)))
            (when info?
              (mprintf "new best size: ~a\n" new-tree-cost #:color 'green))
            (set! best-tree      new-tree)
            (set! best-seq-time  new-seq-time)
            (set! best-tree-cost new-tree-cost)
            (set! best-trans-seq new-trans-seq))
          (loop new-tree
                new-seq-time (+ depth 1) (+ cost (* tdiff new-inv-proba)) new-inv-proba
                new-trans-seq))))
    (define post-loop (current-milliseconds))
    (define remaining-time (- time-limit (- post-loop pre-DFS)))
    (define next-cost-limit
      (min (* 2 cost-limit) remaining-time))
    #;(newline)
    #;(debug remaining-time next-cost-limit)
    (when (> next-cost-limit 1)
      (when info?
        (define loop-time (- post-loop pre-loop))
        (debug loop-time remaining-time next-cost-limit))
      (DFS next-cost-limit)))
  (when info?
    (debug best-trans-seq best-tree-cost))
  ;; Construct and return the return value
  (rich-return
   return
   `((tree . ,best-tree)
     (time . ,best-seq-time)
     (cost . ,best-tree-cost)
     (trans . ,(map (λ (t) (dict-ref trans-dict t)) (reverse best-trans-seq)))
     (trans-names . ,(reverse best-trans-seq)))))

;; Use this to apply the 'trans-seq result of `smart-simplify` to the original tree.
(define (apply-trans-seq tree trans-seq)
  (foldl (λ (op t) (op t)) tree trans-seq))

;; Example:
(module+ main #;drracket
  (require "derivative.rkt"
           "arithmetic.rkt"
           "special-functions.rkt")
  
  (define df (derivative (* (+ 1 'x) (+ 2 'x) (gamma (+ 3 'x)) (exp (+ 4 'x))
                             (sqr (+ 5 'x)) (^ (log (+ 6 'x)) -1))
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
  (displayln (list 'ddf-simple-tree-size: (time (smart-simplify ddf #:return 'cost))))
  )

#;
(begin
  (require rascas)
  (smart-simplify
   #:return 'trans-names
   (+ (* 'a 'b) (exp (* 'a 'b)) (cos (* 'a 'b)) (sin (* 'a 'b))))
  )

