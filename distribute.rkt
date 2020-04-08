#lang racket/base

(require "misc.rkt"
         "automatic-simplify.rkt"
         racket/list)

(provide distribute)

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

;; Recursively distributes `times` over `plus`, where `times` and `plus` are
;; two function symbols like '* and '+ and must have been registered with
;; register-simple-function.
;; WARNING: This is a dangerous function to call. It can blow up in both space and time.
(define (distribute u times plus)
  ; (times a (plus b c)) =
  ; (plus (times a b) (times a c))
  ; applied recursively
  (define times-fun (symbol->function times))
  (define plus-fun (symbol->function plus))
  (define ures
    (let loop ([u u])
      (if (nonempty-list? u)
        (let ([u (map loop u)])
          (if (eq? (first u) times)
            (apply plus-fun
                   (map
                    (λ (l) (apply times-fun l))
                    (apply cartesian-product
                           (map
                            (λ (v)
                              (if (and (nonempty-list? v)
                                       (eq? (first v) plus))
                                (rest v)
                                (list v)))
                            (rest u)))))
            (let ([fun (symbol->function (first u))])
              (if fun
                (apply fun (rest u))
                u))))
        u)))
  (if (equal? ures u)
    u
    ; May need more distribution. Seems to me that there should be a better way
    (distribute ures times plus)))

(module+ test
  (require rackunit
           "arithmetic.rkt"
           "quadratic.rkt")
  (check-equal?
   (distribute '(* f (+ a b) (+ c d) e) '* '+)
   '(+ (* a c e f) (* b c e f) (* a d e f) (* b d e f)))
  (check-equal?
   (distribute '(+ (* f (+ a b) (+ c d) e)) '* '+)
   '(+ (* a c e f) (* b c e f) (* a d e f) (* b d e f)))
  (check-equal?
   (distribute '(* (+ (* (+ a b)
                         c)
                      d)
                   (+ e f))
               '* '+)
   '(+ (* a c e) (* b c e) (* d e) (* a c f) (* b c f) (* d f)))

  (check-equal?
   (distribute
    (apply *
           (map (λ (r) (- 'x r))
                (solve-quadratic 'u 'v 'w)))
    '* '+)
   '(+ (* (^ u -1) w)
       (* (^ u -1) v x)
       (^ x 2))))

; Strange output. It looks like lexicographical from the end, and not placing lists last.
#;
(distribute '(* (+ 1 a) (+ 1 b) (+ 1 c) (+ 1 d)) '* '+)
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