#lang racket
(require rascas
         (except-in plot inverse))

;;;; Same example as AutoGrad:
;;;; https://github.com/HIPS/autograd

; Almost like tanh but nicer to plot.
(define fx 
  (_let* `([y ,(exp 'x)])
    (/ (- 'y 1.)
       (+ 'y 1.))))

;; Construct 6 derivatives.
(define dfs
  (for/fold ([dfs (list fx)])
            ([i 6])
    (cons (contract-let* (derivative (first dfs) 'x))
          dfs)))

;; Plot them.
(parameterize ([plot-decorations? #f])
  (send
   (plot-frame
    (for/list ([df (in-list dfs)] [i (in-naturals)])
      (function (tree->procedure df 'x #:inexact? #t)
                -7 7 #:color (+ i 1))))
   show #t))
