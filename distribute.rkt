#lang racket/base

(require "misc.rkt"
         "automatic-simplify.rkt"
         racket/list)

(provide distribute
         distribute-product)

(define (distribute-product u)
  (distribute u '* '+))

;; 1.5× faster than the built-in one, but the order
;; of the resulting lists is reversed (the order within each list
;; is the same though).
(define (cartesian-product . ls)
  ; We have already checked before that these are lists
  #;(for ([(l i) (in-indexed ls)])
    (unless (list? l)
      (apply raise-argument-error 'cartesian-product "list?" i ls)))
  (define (cp-2 as bs)
    #;(for*/list ([i (in-list as)] [j (in-list bs)]) (cons i j))
    ; Faster variant:
    (let aloop ((as as) (res '()))
      (if (null? as)
        (values res)
        (let ((a (car as)) (ras (cdr as)))
          (let bloop ((bs bs) (res res))
            (if (null? bs)
              (aloop ras res)
              (let ((b (car bs)) (rbs (cdr bs)))
                (bloop rbs (cons (cons a b) res)))))))))
  (foldr cp-2 (list (list)) ls))

(define ((no-fun sym) . l)
  (cons sym l))

;; Recursively distributes `times` over `plus`, where `times` and `plus` are
;; two function symbols like '* and '+ and must have been registered with
;; register-function.
;; If no function is found, the distribution is still applied without applying
;; the functions.
;; WARNING: This function does *not* test whether times and plus have the distribution
;; property, hence calling it with two operators that don't distribute will likely
;; result in erroneous output.
;; WARNING: This is a dangerous function to call. It can blow up in both space and time.
(define (distribute u times plus)
  ; (times a (plus b c)) =
  ; (plus (times a b) (times a c))
  ; applied recursively
  (define times-fun (or (symbol->function times) (no-fun times)))
  (define plus-fun  (or (symbol->function plus)  (no-fun plus)))
  (define ures
    (let loop ([u u])
      (if (nonempty-list? u)
        (let ([u (map loop u)])
          (if (eq? (first u) times)
            (let ([prods
                   (map
                    (λ (l) (apply times-fun l))
                    (apply cartesian-product
                           (map
                            (λ (v)
                              (if (and (nonempty-list? v)
                                       (eq? (first v) plus))
                                (rest v)
                                (list v)))
                            (rest u))))])
              (cond [(empty? prods)
                     (times-fun)]
                    [(empty? (rest prods))
                     (first prods)]
                    [else
                     (apply plus-fun prods)]))
            ; else, not a 'times, apply the function is found to attempt
            ; some reduction that may lessen the burden of the distribution.
            (let ([fun (symbol->function (first u))])
              (if fun
                (apply fun (rest u))
                u))))
        u)))
  ;(displayln ures) (read-line)
  (if (equal? ures u)
    u
    ; May need more distribution. Seems to me that there should be a better way
    (distribute ures times plus)))

(module+ test
  (require rackunit
           "arithmetic.rkt"
           "quadratic.rkt")

  (check-equal?
   (distribute '(PLUS (TIMES (PLUS a b) (PLUS c d) x)
                      (TIMES x y)
                      z)
               'TIMES
               'PLUS)
   ; The first PLUS is not flattened. Should it be?
   ; This test forces a specific order of the TIMES, but it shouldn't matter.
   '(PLUS (PLUS (TIMES b c x) (TIMES b d x) (TIMES a c x) (TIMES a d x))
          (TIMES x y)
          z))

  
  (check-equal?
   (distribute-product '(* f (+ a b) (+ c d) e))
   '(+ (* a c e f) (* b c e f) (* a d e f) (* b d e f)))
  (check-equal?
   (distribute-product '(+ (* f (+ a b) (+ c d) e)))
   '(+ (* a c e f) (* b c e f) (* a d e f) (* b d e f)))
  (check-equal?
   (distribute-product
    '(* (+ (* (+ a b)
              c)
           d)
        (+ e f)))
   '(+ (* a c e) (* b c e) (* d e) (* a c f) (* b c f) (* d f)))

  (check-equal?
   (distribute-product
    (apply *
           (map (λ (r) (- 'x r))
                (solve-quadratic 'u 'v 'w))))
   '(+ (* (^ u -1) w)
       (* (^ u -1) v x)
       (^ x 2))))

; Strange output. It looks like lexicographical from the end, and not placing lists last.
#;
(distribute-product '(* (+ 1 a) (+ 1 b) (+ 1 c) (+ 1 d)) )
#;
'(+
  1
  a
  b
  (* a b)
  c
  (* a c)
  (* b c)
  (* a b c)
  d
  (* a d)
  (* b d)
  (* a b d)
  (* c d)
  (* a c d)
  (* b c d)
  (* a b c d))
