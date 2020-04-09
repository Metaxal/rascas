#lang racket/base

;; Simple calculator expression parser based on the default one in
;; /usr/share/racket-6.12.0.3/pkgs/parser-tools-lib/parser-tools/examples/calc.rkt
;; An interactive calculator inspired by the calculator example in the bison manual.

(provide alg)

;; Import the parser and lexer generators.
(require racket/port
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR #;FNCT))
(define-empty-tokens op-tokens (SEMICOLON #;newline = OP CP + - * × / ÷ ^ EOF NEG))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))

  (upper-letter (:/ #\A #\Z))

  (greek-lower (:/ #\u03B1 #\u03C9))
  (greek-upper (:/ #\u0391 #\u03A9))

  (letter (:or lower-letter upper-letter greek-lower greek-upper))

 ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
 (digit (:/ "0" "9")))

(define (string->imaginary-number str)
  (make-rectangular
   0
   (string->number (substring str 0 (- (string-length str) 1)))))
 
(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\newline) (calcl input-port)]
   [(:or #\;) (token-SEMICOLON)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^" "×" "÷") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   #;["sin" (token-FNCT sin)]
   [(:+ letter) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   ;; complex number, imaginary part:
   [(:: (:+ digit) #\i) (token-NUM (string->imaginary-number lexeme))]
   ;; floatting point number:
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))
   

(define calcp
  (parser
   
   (start start)
   (end SEMICOLON EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (ok? name val)
            (if ok?
                (raise (error "Parsing error:" name val))
                (raise (error "Invalid token: " name val)))))

   (precs (right =)
          (left - +)
          (left * × / ÷)
          (left NEG)
          (right ^))
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           
           [(exp) $1])
    
    (exp [(NUM) $1]
         [(VAR OP exp CP) (list $1 $3)]
         [(VAR) $1]
         [(exp = exp) (list '= $1 $3)]
         [(exp + exp) (list '+ $1 $3)]
         [(exp - exp) (list '- $1 $3)]
         [(exp * exp) (list '* $1 $3)]
         [(exp × exp) (list '* $1 $3)]
         [(exp / exp) (list '/ $1 $3)]
         [(exp ÷ exp) (list '/ $1 $3)]
         [(- exp) (prec NEG) (list '- $2)]
         [(exp ^ exp) (list '^ $1 $3)]
         [(OP exp CP) $2]
         #;[(exp exp) (list '* $1 $2)]))))
           
;; run the calculator on the given input-port       
(define (calc [ip (current-input-port)])
  (port-count-lines! ip)
  (let loop ([l '()])
    (define result (calcp (lambda () (calcl ip))))
    (if result
        (loop (cons result l))
        (let ([l (reverse l)])
          (cond [(null? l) '()]
                [(null? (cdr l))
                 (car l)]
                [else (cons 'LIST l)])))))

(define (alg str)
  (with-input-from-string str
    calc))

(module+ test
  (require rackunit)
  (check-equal? (alg "f(x)=a*x+b")
                '(= (f x) (+ (* a x) b)))
  (check-equal? (alg "f(a)")
                '(f a))
  (check-equal? (alg "x+3;y-2-3")
                '(LIST (+ x 3)
                       (- (- y 2) 3)))
  (check-equal? (alg "cos(3) + sin(2)")
                '(+ (cos 3) (sin 2)))
  (check-equal? (alg " sin(x)^2 + 2 * sin(x) + 3 ")
                '(+ (+ (^ (sin x) 2)
                       (* 2 (sin x)))
                    3))
  (check-equal? (alg "a + b
                        + c")
                '(+ (+ a b) c))
  (check-equal? (alg "α + β
                        + λ")
                '(+ (+ α β) λ))
  (check-equal? (alg "3+2i")
                '(+ 3 0+2i))
  )
