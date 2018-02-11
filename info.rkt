#lang info
(define collection "rascas")
(define deps '("base"
               "math-lib"
               "parser-tools-lib"
               "rackunit-lib"
               "srfi-lite-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rascas.scrbl" ())))
(define pkg-desc "Computer Algebra System for Racket. Port of dharmatech/mpl.")
(define version "0.0")
(define pkg-authors '(orseau))
