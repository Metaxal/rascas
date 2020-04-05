#lang racket/base
(require "arithmetic.rkt"
         "derivative.rkt"
         "substitute.rkt")

(provide taylor)

(define (taylor f x x0 n)
  (cons '+
        (let loop ([f f] [k 0])
          (if (or (= k (+ n 1))
                  (equal? f 0))
              '()
              (cons (* (^ (- x x0) k)
                       (substitute f x x0)
                       (^ (! k) -1))
                    (loop (derivative f x)
                          (+ k 1)))))))

(module+ test
  (require rackunit)
  
  (check-equal? (taylor '(exp x) 'x 'x0 3)
                '(+
                  (exp x0)
                  (* (exp x0) (+ x (* -1 x0)))
                  (* 1/2 (exp x0) (^ (+ x (* -1 x0)) 2))
                  (* 1/6 (exp x0) (^ (+ x (* -1 x0)) 3)))))