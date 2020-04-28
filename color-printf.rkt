#lang racket/base

(require racket/dict)

(provide (rename-out [color-printf printf])
         current-use-colors?)

;; https://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html

(define current-use-colors? (make-parameter #t))

(define background-colors
  '((black . "\033[40m")
    (blue . "\033[44m")
    (cyan . "\033[46m")
    (darkgray . "\033[100m")
    (green . "\033[42m")
    (lightblue . "\033[104m")
    (lightgray . "\033[47m")
    (lightgreen . "\033[102m")
    (lightpurple . "\033[105m")
    (lightred . "\033[101m")
    (magenta . "\033[45m")
    (orange . "\033[43m")
    (red . "\033[41m")
    (teal . "\033[106m")
    (white . "\033[107m")
    (yellow . "\033[103m")
    (default . "\033[49m")))
(define default-bg (dict-ref background-colors 'default))

(define foreground-colors
  '((black . "\033[30m")
    (blue . "\033[34m")
    (cyan . "\033[36m")
    (green . "\033[32m")
    (lightgray . "\033[37m")
    (magenta . "\033[35m")
    (orange . "\033[33m")
    (red . "\033[31m")
    (default . "\033[39m")))
(define default-fg (dict-ref foreground-colors 'default))

(define (color-printf form #:color [color #f] #:background [bg-color #f] . vs)
  (define fg (and color
                  (dict-ref foreground-colors color
                            (λ () (error "Unknown color" color)))))
  (define bg (and bg-color
                  (dict-ref background-colors bg-color
                            (λ () (error "Unknown background color" bg-color)))))
  (define str (apply format form vs))
  (if (and (or fg bg) (current-use-colors?))
    (display (string-append (or bg "")
                            (or fg "")
                            str
                            (if fg default-fg "")
                            (if bg default-bg "")))
    (display str)))

