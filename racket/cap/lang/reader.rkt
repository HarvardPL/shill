#lang s-exp syntax/module-reader
shill/cap/main
#:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
#:read-syntax this-read-syntax
#:whole-body-readers? #t
#:info get-info
#:language-info '#(shill/private/language-info get-language-info #f)

(require shill/cap/parse)

(define (this-read-syntax [src #f] [in (current-input-port)])
    (parse-program src in))

(define (get-info s default filter)
  (cond [(equal? s 'color-lexer) color-lexer]
        [else (filter s default)]))
        







