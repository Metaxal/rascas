#lang racket/base

(require "misc.rkt"
         "arithmetic.rkt"
         "contains.rkt"
         racket/list
         racket/match)

(provide inverse)

;;; TODO:
;;; solve with one argument. Requires knowing the inverses of each function.
;;; so we need a dictionary for this too.

;;; if an inverse is not know, it should be written as (inverse (...) 'x).

;;; Ex: find the minimum of
#;(+ (/ 1 'x) (sqr 'x))
; Diff:
#;(solve-zero (derivative (+ (/ 1 'x) (sqr 'x)) 'x)
              'x)
#;(solve-zero '(+ (* -1 (^ x -2)) (* 2 x)) 'x)

(define no-inverse 'no-inverse)

;; This works only for functions of 1 argument
(define inverse-dict (make-hasheq))
(define (register-inverse f f-1)
  (hash-set! inverse-dict f f-1)
  (hash-set! inverse-dict f-1 f))
(define (function-inverse sym)
  (hash-ref! inverse-dict sym #f))

(register-inverse 'exp 'log)
(register-inverse 'cos 'acos)
(register-inverse 'sin 'asin)
(register-inverse 'tan 'atan)
(register-inverse 'cosh 'acosh)
(register-inverse 'sinh 'asinh)
(register-inverse 'tanh 'atanh)

;; Returns the inverse of y=u(x) for x.
(define (inverse u x y) ; change order of arguments??
  (let loop ([u (- u y)] [x x] [y 0]) ; just in case y contains x.
    (cond
      [(equal? u x) y]
      [(contains? u x)
       (match u
         [`(^ ,a ,b)
          (define x-in-a? (contains? a x))
          (define x-in-b? (contains? b x))
          (cond [(and x-in-a? x-in-b?)
                 `(inverse ,u ,x ,y)]
                [x-in-a? (loop a x (^ y (/ b)))]
                [x-in-b? (loop (log a) x (log y))]
                [else no-inverse])] ; can't happen, tested earlier
         [`(+ . ,as)
          (define-values (have-xs other)
            (partition (contains-this? x) as))
          (define new-y (- y (apply + other)))
          (match have-xs
            ['() no-inverse]
            [(list a) (loop a x new-y)]
            [else `(inverse (+ . ,have-xs) ,x ,new-y)])] ; don't know what to do yet
         [`(* . ,as)
          (define-values (have-xs other)
            (partition (contains-this? x) as))
          (define new-y (/ y (apply * other)))
          (match have-xs
            ['() no-inverse]
            [(list a) (loop a x new-y)]
            [else (loop (apply + (map log have-xs))
                           x (log y))])] ; try  to reduce to the +
         ; functions of one argument
         [`(,f ,a) (define inv-sym (function-inverse f))
                   (define inv (and inv-sym (symbol->function inv-sym)))
                   ; not sure which is better:
                   (loop a x (inv y))
                   ; maybe this one will check the definition domain automatically,
                   ; when there is such a thing...
                   #;(loop (inv u) x (inv y))]   
         [else `(inverse ,u ,x ,y)])]
      [else no-inverse])))

(module+ test
  (require rackunit)
  (check-equal? (inverse '0 'x '(+ x 1)) -1)
  (check-equal? (inverse (/ (log (+ 1 (* 'α (- 'x 1)))) 'α) 'x 'y)
                (+ 1(/ (- (exp (* 'y 'α)) 1) 'α))))
