#lang racket


(provide shill-provide
         val
         var
         set-var
         shill-require
         shill-for-acc
         shill-for
         (contract-out [++ ++/c]))

(require (for-syntax syntax/parse syntax/id-table syntax/modresolve racket/require-transform "../cap/parse.rkt"))
;; switch-on/off contracts 
(define-for-syntax WITH-CONTRACTS #t)

(define-syntax shill-provide
  (if WITH-CONTRACTS
      (λ (stx)
        (syntax-parse stx
                      [(_ spec ...) #'(provide (contract-out spec ...))]))
      (λ (stx)
        (define-syntax-class clause
          #:attributes (i)
          (pattern [i:id ctc:expr]))
        (syntax-parse stx
                      [(_ c:clause ...) #'(provide c.i ...)]))))
                       
  
(define-syntax-rule (shill-require spec ...) (require (check-if-shill spec) ...))


(define-syntax check-if-shill
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ spec)
        (let-values ([(imports sources) (expand-import #'spec)])
          (values
           (begin
             (for/fold ([seen null])
               ([id imports])
               (let* ([src-mod (import-src-mod-path id)]
                      [src-mod-path (resolve-module-path (cond [(module-path? src-mod) src-mod]
                                                               [else (syntax->datum src-mod)])
                                                          #f #;(syntax-source #'spec))]
                      [mod-info 
                       (module->language-info src-mod-path)])
                 (cond [(member src-mod-path seen)
                        seen]
                       [(and (equal? mod-info '#("../private/language-info.rkt" get-language-info #f))
                             (with-handlers ([exn:fail:syntax?
                                              (λ (e) 
                                                 (error 
                                                  'non-shill-module-required 
                                                  "~a not a shill/cap module"
                                                  src-mod-path))])
                               (parse-program #f (open-input-file src-mod-path))))
                        (cons src-mod-path seen)]
                       [else (error 
                              'non-shill-module-required 
                              "~a not a shill/cap module"
                              src-mod-path)]))) 
             imports)
           sources))]))))

(define-syntax-rule (val x v) (define x v))

(define-for-syntax setable-identifiers (make-free-id-table))

(define-syntax (set-var stx)
  (syntax-case stx ()
    [(_ id v)
     (if (free-id-table-ref setable-identifiers #'id #f)
         #'(set! id v)
         #'(error 'set-var
                  "~a is not mutable, it must be declared with var."
                  (symbol->string 'id)))]))

(define-syntax (var stx)
  (syntax-case stx ()
    [(_ id v)
     (begin (free-id-table-set! setable-identifiers #'id #t)
            #'(define id v))]))

(define-for-syntax (allign init updates)
  (define (get-updated id l)
    (define maybe-updated? (assoc id l))
    (if maybe-updated? (cadr maybe-updated?) id))
  (foldr (λ (x y) (cons (get-updated (car x) updates) y)) null init))

(define-syntax (shill-for-acc stx)
  (syntax-case stx ()
    [(shill-for iters accs body ... updates)
     (with-syntax ([(new-update ...)
                    (datum->syntax #'updates (allign (syntax->datum #'accs) (syntax->datum #'updates)))])
       #'(for/fold accs iters (let () body ... (values new-update ... ))))]))

(define-syntax (shill-for stx)
  (syntax-case stx ()
    [(shill-for clauses h t ...)
     #'(for clauses (let () h t ... ))]))

(define ++/c
  (->i ([a (or/c string? list?)]
        [b (a) (λ (x)
                 (or (and (string? x) (string? a)) 
                     (and (list? x) (list? a))))])
       any))

(define (++ a b)
  (cond [(list? a) (append a b)]
        [else (string-append a b)]))
