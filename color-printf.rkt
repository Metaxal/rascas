#lang racket/base

(require racket/dict
         racket/string
         racket/port
         racket/system)

(provide (rename-out [color-printf printf]
                     [color-format format])
         current-use-colors?)

;; https://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html

;; Query how many colors are supported. Any way to avoid system*?
(define (n-supported-colors)
  (define out (open-output-string))
  (define res
    (parameterize ([current-output-port out]
                   [current-error-port (open-output-string)])
      (system* "/usr/bin/tput" "colors")))
  (string->number (string-normalize-spaces (get-output-string out))))

(define current-use-colors?
  ;; Check if the program is running in a terminal that supports colors.
  (make-parameter (n-supported-colors)))

(define background-colors
  '((black . "\e[40m")
    (blue . "\e[44m")
    (cyan . "\e[46m")
    (darkgray . "\e[100m")
    (green . "\e[42m")
    (lightblue . "\e[104m")
    (lightgray . "\e[47m")
    (lightgreen . "\e[102m")
    (lightpurple . "\e[105m")
    (lightred . "\e[101m")
    (magenta . "\e[45m")
    (orange . "\e[43m")
    (red . "\e[41m")
    (teal . "\e[106m")
    (white . "\e[107m")
    (yellow . "\e[103m")
    (default . "\e[49m")))
(define default-bg (dict-ref background-colors 'default))

(define foreground-colors
  '((black . "\e[30m")
    (blue . "\e[34m")
    (cyan . "\e[36m")
    (green . "\e[32m")
    (lightgray . "\e[37m")
    (magenta . "\e[35m")
    (orange . "\e[33m")
    (red . "\e[31m")
    (default . "\e[39m")))
(define default-fg (dict-ref foreground-colors 'default))

(define (color-format form #:color [color #f] #:background [bg-color #f] . vs)
  (define fg (and color
                  (dict-ref foreground-colors color
                            (λ () (error "Unknown color" color)))))
  (define bg (and bg-color
                  (dict-ref background-colors bg-color
                            (λ () (error "Unknown background color" bg-color)))))
  (define str (apply format form vs))
  (if (and (or fg bg) (current-use-colors?))
    (string-append (or bg "")
                   (or fg "")
                   str
                   (if fg default-fg "")
                   (if bg default-bg ""))
    str))

(define (color-printf form #:color [color #f] #:background [bg-color #f] . vs)
  (display (apply color-format form #:color color #:background bg-color vs)))

