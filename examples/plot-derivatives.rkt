#lang racket
(require rascas
         (except-in plot inverse))

;;;; Same example as AutoGrad:
;;;; https://github.com/HIPS/autograd

; Almost like tanh but nicer to plot.
(define fx
  (/ (- (exp 'x) 1.)
     (+ (exp 'x) 1.)))

;; Build derivatives up to the 6th order.
(define dfs
  (for/fold ([dfs (list fx)])
            ([i 6 #;8])
    (cons (smart-simplify (derivative (first dfs) 'x))
          dfs)))

;; Plot them.
(parameterize ([plot-decorations? #f])
  (send
   (plot-frame
    (for/list ([df (in-list dfs)] [i (in-naturals)])
      (function (tree->procedure df 'x #:inexact? #t)
                -7 7 #:color (+ i 1) #:samples 200 #:width 2)))
   show #t))
