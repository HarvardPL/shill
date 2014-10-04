#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file contains definitions of functions, properties and structures that ; 
; are useful for different pieces of the shill implementation.                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide enhance-blame/c
         mutator-redirect-proc
         prop:rights
         rights?
         get-rights
         prop:shadow-details
         shadow-details?
         get-shadow-details
         prop:parameterized
         parameterized?
         get-parameter
         param
         param?
         param-make
         param-value
         check-compatible)

(struct enhance-blame/c (ctc msg)
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
     (define inner-ctc (enhance-blame/c-ctc ctc))
     (define msg (enhance-blame/c-msg ctc))
     (λ (blame)
       (define new-blame 
         (blame-add-context 
          blame 
          (string-append msg " (insufficient privileges for " msg "!) in")))
       (λ (val)
         (((contract-projection inner-ctc) new-blame) val))))))

(define (mutator-redirect-proc i v) v)

(define-values (prop:rights rights? get-rights)
  (make-impersonator-property 'rights))

(define-values (prop:shadow-details shadow-details? get-shadow-details)
  (make-impersonator-property 'shadow-details))

(define-values (prop:parameterized parameterized? get-parameter)
  (make-impersonator-property 'parameterized))

(struct param (make value))


(define (check-compatible compatible? new-rights old-rights blame val kind)
  (define extra
    (cond [(string=? kind "pipe-factory")
           " for the first and second pipe-end respectively"]
          [else ""]))          
  (cond [(procedure? old-rights) old-rights]
        [(or (false? old-rights) (compatible? new-rights old-rights)) new-rights]
        [else
         (thunk 
          (raise-blame-error 
           blame 
           val 
           (list 'expected
                 kind
                 (string-append "with at least ~a unconstrained privileges" extra)
                 'given
                 (string-append "~a with ~a unconstrained privileges" extra)
                 extra)
           (rights->string new-rights kind)
           val
           (rights->string old-rights kind)))]))

(define (rights->string rights kind)
  (cond [(string=? kind "pipe-factory")
         (string-append 
          (node-rights->string (first rights))
          " and "
          (node-rights->string (second rights)))]
        [(string=? kind "socket-factory") (socket-factory-rights->string rights)]
        [else (node-rights->string rights)]))

(define (node-rights->string rights)
  (define (nested-rights->string right)
    (cond [(symbol? right) (string-append "+" (symbol->string right))]
          [else (let ([maybe-nested (second right)])
                  (if maybe-nested
                      (string-append "+"(symbol->string (first right)) " [" (node-rights->string (second right)) "]")
                      (string-append "+" (symbol->string (first right)))))]))
  (cond [(boolean? rights) "no"]
        [(empty? rights) ""]
        [(empty? (rest rights)) (nested-rights->string (first rights))]
        [else (string-append 
               (nested-rights->string (first rights))
               ", "
               (node-rights->string (rest rights)))])) 

(define (socket-factory-rights->string rights)
  (define (family->string family) (format "~e" family))
  (define (address-list->string addl) 
    (define (number-pair->string pair) (format "[~e,~e]" (first pair) (second pair)))
    (define (number-pair-list->string pairl) 
      (cond [(empty? pairl) "any"]
            [else (foldr 
                   (λ (h t) (string-append 
                             (number-pair->string h) 
                             (if (string=? "" t) "" ", ")
                             t)) 
                   "" 
                   pairl)]))
    (define (address-list-element->string addlel)
      (string-append "[" (number-pair->string (first addlel)) " : " (number-pair-list->string (second addlel)) "]"))
    (cond [(empty? addl) "any"]
          [else (foldr 
                 (λ (h t) (string-append 
                           (address-list-element->string h) 
                           (if (string=? "" t) "" ", ")
                           t))
                 ""
                 addl)]))
  (string-append
   (family->string (first rights))
   " * "
   (family->string (second rights))
   " * "
   (address-list->string (third rights))
   " * ("
   (node-rights->string (fourth rights))
   ")"))
