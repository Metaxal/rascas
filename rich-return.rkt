#lang racket/base

(require racket/dict
         racket/list
         racket/match
         syntax/parse/define)

(provide rich-return
         rich-return/ids
         with-return-values
         define-return-values)

;;;; This deserves a library of its own.

;; Allows the user to have control on the return values
;; res-dict: dict? (hence possibly hash, vector, assoc list)
;; TODO struct with fields? (prefab?)
(define (rich-return return res-dict)
  (unless (dict? res-dict)
    (raise-argument-error 'rich-return "dict?" res-dict))
  (let loop ([return return])
    (match return
      ['dict res-dict]
      ['list (dict-values res-dict)]
      ['values (apply values (dict-values res-dict))]
      [(? symbol?)
       (dict-ref res-dict return
                 (λ () (error
                        'rich-return
                        "Unknown return value: ~a\nExisting keys: ~a\nOther allowed values: ~a\n"
                        return (dict-keys res-dict) '(dict list values))))]
      [`(dict ,(? symbol? syms) ...)
       ; Choose which keys are returned
       (map (λ (s) (cons s (loop s))) syms)]
      [`(list ,args ...)
       (map loop args)]
      [`(values ,args ...)
       (apply values (map loop args))]
      [(list args ...)
       (map loop args)])))

(define-simple-macro (rich-return/ids return:expr id:id ...)
  (rich-return return (list (cons 'id id) ...)))

;; A convenience form to avoid typing ids several times when re-using the result
;; of a return.
;; Requires the function to use an _optional_ #:return keyword.
;; See examples below.
(define-simple-macro (with-return-values
                         [(id:id ...) (callee:expr arg ...)]
                       body:expr ...)
  (let-values ([(id ...) (callee #:return '(values id ...) arg ...)])
    body ...))

(define-simple-macro (define-return-values (id:id ...) (callee:expr arg ...))
  (define-values (id ...) (callee #:return '(values id ...) arg ...)))

;; TODO: Provide a define/return form that is able to check at compile time if the requested
;; return values exist so as to fail before running a long program.
;; If not, then check at the entry of the function instead of at the end.

(module+ test
  (require rackunit)
  (check-exn exn:fail?
             (λ () (rich-return 'auei '((béop . 3) (nstn . w)))))
  (check-exn exn:fail?
             (λ () (rich-return 'auei 3)))

  (let ()
    (define (foo [x 10] #:return [return 'dict] . vs)
      (define time 30)
      (define user 40)
      (rich-return/ids return time user x vs))
    (check-equal? (with-return-values [(time) (foo)]
                    time)
                  30)
    (check-equal? (foo)
                  '((time . 30) (user . 40) (x . 10) (vs)))
    (check-equal? (foo #:return 'list)
                  '(30 40 10 ()))
    (check-equal? (foo #:return 'user)
                  40))
  )

(module+ drracket
  ;; Add a keyword argument so that the caller can request the return type.
  (define (foo #:return [return 'sum])
    (newline)
    (writeln return)

    ;; Some user code with several interesting values
    (define-values (imax sum cpu real gc)
      ; long computation...
      (values (random 100) (random 10000) (random 200) (random 200) (random 100)))
    
    ; Now we can return the values asked by the caller
    (rich-return/ids return imax sum cpu real gc))
  
  (foo)
  (foo #:return 'dict)
  (foo #:return 'list)
  (foo #:return 'values)
  (foo #:return '(values cpu sum))
  (foo #:return '(dict gc gc imax))
  (foo #:return '(values (dict gc sum) sum imax))
  (with-return-values [(gc sum) (foo)]
    (printf "gc: ~a sum: ~a\n" gc sum))


  ;; Note:
  #;(rich-return/ids return imax sum cpu real gc)
  ; is equivalent to
  #;(rich-return return
                 `((imax . ,imax)
                   (sum . ,sum)
                   (cpu . ,cpu)
                   (real . ,real)
                   (gc . ,gc))))

