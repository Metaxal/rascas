#lang racket/base

(require rascas/misc
         rascas/arithmetic
         rascas/order-relation
         racket/list
         racket/match
         data/heap
         (prefix-in rkt: (only-in racket/base + * - /)))

#;
(define (chain-comparators a b kind+cmps)
  (if (empty? kind+cmps)
    (error "cmps empty" 'order<?)
    (let ([kind+cmp (first kind+cmps)])
      (define kind (first kind+cmp))
      (define cmp (second kind+cmp))
      (define kinda (kind a))
      (define kindb (kind b))
      (cond [(and kinda kindb)
             (cmp a b)]
            [kinda #t]
            [kindb #f]
            [else (chain-comparators a b (rest kind+cmps))]))))


(module+ main
  (define n 1000000)
  (define l (shuffle (append* (build-list n (λ (i) (random 1000)))
                              (make-list n '(x y z t u v)))))
  (collect-garbage)
  (time (apply + l))
  )
