#lang racket/base

(provide configure)

(require shill/ambient/parse)

(define (configure data)
  (current-read-interaction
   (lambda (src port)
     (if (char-ready? port)
         (parameterize ([read-accept-reader #f]
     	                [read-accept-lang #f])
         (let* ([result (parse-program src port)]
                [new (datum->syntax #f (syntax->datum (car (syntax->list result)))
	                (list (syntax-source result)		 
			      (syntax-line result)		 
			      (syntax-column result)		 
			      (syntax-position result)	 
			      (syntax-span result)))])	 
	    new))
	 eof))))