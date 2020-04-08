#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide tan)

(require "misc.rkt"
         (prefix-in rkt: (only-in racket/base tan)))

(define-simple-function tan rkt:tan)
