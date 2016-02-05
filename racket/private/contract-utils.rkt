#lang racket

(require "utils.rkt"
         "filesystem.rkt"
         "net.rkt")

(provide forall/c
         cap?
         (rename-out [shill-wallet? wallet?])
         create-wallet
         get
         put
         wallet/c)

(define-syntax (forall/c stx)
  (syntax-case stx ()
    [(_ ([x p]...) ctc)
     (begin
       (for ([x (in-list (syntax->list #'(x ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'forall/c 
                               "expected an identifier"
                               stx
                               x)))
       #'(forall tag/c '(x ...) (list p ...) (lambda (x ...) ctc)))]))

(define (tag/c positive? name details)
  (define-values [type make pred getter setter]
    (make-struct-type name #f 1 0))
  (define (get x) (getter x 0))
  (seal-unseal/c name positive? make pred get details))

(struct forall [tag vars privileges body]
  #:property prop:contract
  (build-contract-property
   #:name
   (λ (ctc)
     `(forall/c ,(forall-vars ctc) ...))
   #:projection
   (λ (ctc)
     (λ (blame)
       (define negative? (blame-swapped? blame))
       (define tag/c (forall-tag ctc))
       (define instances
         (for/list ([var (in-list (forall-vars ctc))]
                    [details (in-list (forall-privileges ctc))])
           (tag/c negative? var details)))
       (λ (val)
         (((contract-projection (apply (forall-body ctc) instances)) blame) val))))))


(define-struct seal-unseal/c [name positive? make pred get details]
  #:property prop:contract
  (build-contract-property
   #:name (lambda (c) (seal-unseal/c-name c))
   #:first-order (λ (c) (seal-unseal/c-pred c))
   #:projection
   (lambda (c)
     (define make (seal-unseal/c-make c))
     (define pred (seal-unseal/c-pred c))
     (define get (seal-unseal/c-get c))
     (define details (seal-unseal/c-details c))
     (lambda (blame)
       (if (equal? (blame-original? blame) (seal-unseal/c-positive? c))
           (λ (val)
             (define current-param (param make (make val)))
             (define (sealed-proj maker) (((contract-projection (maker details current-param)) blame) val))
             (cond [(socket-factory? val) (sealed-proj socket-factory-proxy)]
                   [(pipe-factory? val) (sealed-proj pipe-factory-proxy)]
                   [(file? val) (sealed-proj file-proxy)]
                   [(dir? val) (sealed-proj dir-proxy)]))
           (λ (val)
             (if (and (parameterized? val) (param? (get-parameter val))) 
                 (let ([inner-value (param-value (get-parameter val))])     
                   (if (pred inner-value)
                       (get inner-value)
                       (raise-blame-error blame val '(expected: "~a" given: "~e")
                                          (seal-unseal/c-name c)
                                          val)))
                 (raise-blame-error blame val '(expected: "~a" given: "~e")
                                    (seal-unseal/c-name c)
                                    val))))))))

; Capability wallets
(struct shill-wallet
  (cap-map [put #:mutable] [get #:mutable]))

(define (wallet/c #:put [put #f]
                  #:get [get #f])
  
  (define put/c (if put 
                    (enhance-blame/c 
                     (->i ([key string?] [val (key) (put key)]) any)
                     "put")
                    (enhance-blame/c
                     (->* (any/c any/c) #:pre #f any)
                     "put")))
  
  (define get/c (if get 
                    (enhance-blame/c
                     (->i ([key string?]) [_ (key) (listof (get key))])
                     "get")
                    (enhance-blame/c
                     (->* (any/c) #:pre #f any)
                     "get")))
  
  (struct/c shill-wallet
            (hash/c string? cap?)
            put/c
            get/c))

(define (create-wallet)
  (define hash (make-hash))
  (shill-wallet hash
                (λ (key cap) (hash-update! hash key (λ (cur) (cons cap cur)) empty))
                (λ (key) (reverse (hash-ref hash key '())))))

(define (get wallet key)
  ((shill-wallet-get wallet) key))

(define (put wallet key cap)
  ((shill-wallet-put wallet) key cap))
