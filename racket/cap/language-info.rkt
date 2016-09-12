#lang racket

 
(provide get-language-info)
 
(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#(shill/cap/lang/runtime-config configure #f))]
      [else default])))