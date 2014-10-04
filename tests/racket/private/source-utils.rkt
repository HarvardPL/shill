#lang racket

(require rackunit shill/private/source-utils "../test-utils.rkt")

(test-case "set-var a val"
           
           (val foo 0)
           (check-exn #rx"foo is not mutable" (lambda () (set-var foo 1))))

(test-case "set-var a var"
           
           (var foo 0)
           (check-not-exn (lambda () (set-var foo 1)))
           (check-equal? foo 1))

(test-case "set-var a top-level var, before its defined"
           
           (val f (lambda () (set-var foo 1)))
           (var foo 0)
           (check-not-exn (lambda () (f)))
           (check-equal? foo 1))

(test-case "set-var a lambda-bound argument identifier"
           
           (val f (lambda (x) (set-var x 1) x))
           (check-exn #rx"x is not mutable" (lambda () (f 0))))

(test-case "shill-for"
           
	   (define result
	     (shill-for ([x '(1 2 3)]) x))
	   
	   (check-equal? result (void)))

(test-case "shill-for-acc"

	   (define-values (x y h z)
	      (shill-for-acc ([x '(1 2 3)]) ([i 0] [y 3] [h 7] [z 4] ) ([y 5] [z 8] [i (+ i x)])))
           
           (check-equal? x 6)
           (check-equal? y 5)
           (check-equal? h 7)
           (check-equal? z 8))

(test-pass 
 "shill-provide"
 (script
  (module provider racket
    (require shill/private/source-utils)
    (shill-provide [foo (-> boolean? boolean?)])
    (define (foo x) x))
  (module consumer racket
    (require shill/private/filesystem 'provider)
    (foo #t))
  (require 'consumer)))
