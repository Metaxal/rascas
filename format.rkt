#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/string)

(provide ->string)

(define ->string-fun-brackets          (make-parameter '("(" ")")))
(define ->string-fun-arg-sep           (make-parameter ", "))
;; Surrounds the operator in infix position
(define ->string-op-arg-sep            (make-parameter ""))
;; first 2 brackets are necessary to avoid ambiguity,
;; next 2 brackets are for optional bracketing
(define ->string-op-sub-expr-brackets  (make-parameter '("(" ")" "" "")))

(define (1char-symbol? c)
  (and (symbol? c)
       (let ([s (symbol->string c)])
         (and (= 1 (string-length s))
              (not (char-alphabetic? (car (string->list s))))))))

(define (->string t)
  (match t
    [(list op xs ...)
     (if (1char-symbol? op)
       (let ([brackets (->string-op-sub-expr-brackets)])
         (string-join (map (λ (x) (if (list? x)
                                    (string-append (first brackets)
                                                   (->string x)
                                                   (second brackets))
                                    (string-append (third brackets)
                                                   (->string x)
                                                   (fourth brackets))))
                           xs)
                      (~a (->string-op-arg-sep) op (->string-op-arg-sep))))
       (string-append (~a op)
                      (first (->string-fun-brackets))
                      (string-join (map ->string xs) (->string-fun-arg-sep))
                      (second (->string-fun-brackets))))]
    [else (~a t)]))

(module+ test
  (require rackunit)
  (check-equal? (->string '(f a b c))
                "f(a, b, c)")
  (parameterize ([->string-fun-arg-sep ","])
    (check-equal? (->string '(f a b c))
                  "f(a,b,c)"))
  (check-equal? (->string '(f (+ 1 2 3) b (* 2 x)))
                "f(1+2+3, b, 2*x)")
  (parameterize ([->string-op-sub-expr-brackets '("(" ")" "{" "}")])
    (check-equal? (->string '(f (+ 1 2 3) b (* 2 x)))
                  "f({1}+{2}+{3}, b, {2}*{x})"))
  (parameterize ([->string-op-arg-sep " "])
    (check-equal? (->string '(f (+ 1 2 3) b (* 2 x)))
                  "f(1 + 2 + 3, b, 2 * x)")))

#;
(->string
 '(*
  (+ 122 x)
  (+ 133 x)
  (+ 247 x)
  (+ 262 x)
  (+ 340 x)
  (+ 607 x)
  (+ 795 x)
  (+ 918 x)
  (+ 921 x)
  (+ 958 x)
  (+
   (^ (+ 122 x) -1)
   (^ (+ 133 x) -1)
   (^ (+ 247 x) -1)
   (^ (+ 262 x) -1)
   (^ (+ 340 x) -1)
   (^ (+ 607 x) -1)
   (^ (+ 795 x) -1)
   (^ (+ 918 x) -1)
   (^ (+ 921 x) -1)
   (^ (+ 958 x) -1))))

#;
(parameterize ([->string-op-arg-sep " "])
  (->string
   '(*
     (+
      (* 2 (^ 2 (* 2 x)) (+ -1 (^ 2 x)) (^ (+ 1 (^ 2 x)) -3))
      (* (^ 2 x) (^ (+ 1 (^ 2 x)) -1))
      (* -1 (^ (+ 1 (^ 2 x)) -2) (+ (* 2 (^ 2 (* 2 x))) (* (^ 2 x) (+ -1 (^ 2 x))))))
     (^ (log 2) 2))))

