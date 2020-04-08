#lang racket/base

(require racket/match
         racket/format
         racket/string)

(define (->string u)
  (match u
    [`(+ . ,elts)
     (string-append "(" (string-join (map ->string elts) "+") ")")]
    [`(* . ,elts)
     (string-append "(" (string-join (map ->string elts) "*") ")")]
    [`(^ ,v ,w)
     (string-append
      "(" (->string v) ")^"
      "(" (->string w) ")")]
    [`(,f . ,elts)
     (string-append
      (->string f)
      "("
      (string-join (map ->string elts) ",")
      ")")]
    [else (~a u)]))

(module+ test
  (require rackunit)
  (check-equal? (->string '(f a b c))
                "f(a,b,c)")
  (check-equal? (->string '(f (+ 1 2 3) b (* 2 x)))
                "f((1+2+3),b,(2*x))"))

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