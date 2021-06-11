#lang racket/base
(require "arithmetic.rkt"
         "derivative.rkt"
         "substitute.rkt"
         "smart-simplify.rkt")

(provide taylor
         nth-derivative)
#;
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

;; n = derivative order. n=0 is f(x0).
;; remainder: (or/c 'laplace #t #f)
;; https://en.wikipedia.org/wiki/Taylor%27s_theorem#Explicit_formulas_for_the_remainder
(define (taylor f x x0 n #:remainder [remainder #f] #:remainder-symbol [rem-sym 'α])
  (define-values (tayl ff)
    (for/fold ([s (substitute f x x0)] [f f])
              ([k (in-range 1 (+ n 1))])
      (define ff (smart-simplify (derivative f x)))
      (values (+ s (/ (* (expt (- x x0) k)
                         (substitute ff x x0))
                      (factorial k)))
              ff)))
  (define rem
    (case remainder
      [(laplace #t)
       (/ (* (expt (- x x0) (+ n 1))
             (substitute (derivative ff x)
                         x
                         (+ x0 (* rem-sym (- x x0)))))
          (factorial (+ n 1)))]
      [else 0]))
  (+ tayl rem))

(define (nth-derivative f x [n 0])
  (if (= n 0)
    f
    (nth-derivative (smart-simplify (derivative f x)) x (- n 1))))

(module+ test
  (require rackunit
           rascas/trig-functions)

  (check-equal? (taylor (log (+ 1 'z)) 'z 0 4)
                '(+ z (* -1/2 (^ z 2)) (* 1/3 (^ z 3)) (* -1/4 (^ z 4))))
  (check-equal? (taylor (log 'z) 'z 1 4)
                '(+ -1 z (* -1/2 (^ (+ -1 z) 2)) (* 1/3 (^ (+ -1 z) 3)) (* -1/4 (^ (+ -1 z) 4))))
  (check-equal? (taylor (sin 'x) 'x 0 5)
                '(+ x (* -1/6 (^ x 3)) (* 1/120 (^ x 5))))

  (check-equal? (taylor '(exp x) 'x 'x0 3)
                '(+
                  (exp x0)
                  (* (exp x0) (+ x (* -1 x0)))
                  (* 1/2 (exp x0) (^ (+ x (* -1 x0)) 2))
                  (* 1/6 (exp x0) (^ (+ x (* -1 x0)) 3)))))

;; Example
(module+ drracket
  (require plot
           "automatic-simplify.rkt")
  (plot
   #:width 800
   #:height 600
   (list (function (λ (x) (log (->inexact x))) 0 4
                   #:label "log")
         (for/list ([i (in-range 4)])
           (define f (taylor '(log x) 'x 1 i))
           (function (λ (x) (->inexact (substitute f 'x x)))
                     #:color (+ i 2)
                     #:label (format "Approx. log at x=1, order ~a" i))))))
