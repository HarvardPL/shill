#lang racket

 
(provide get-language-info)
 
(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#(racket/runtime-config configure #f))]
      [else default])))