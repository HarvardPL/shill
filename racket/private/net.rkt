#lang racket

(require shill/private/utils)

(provide (rename-out [make-socket-factory socket-factory])
         socket-factory?
         socket-factory/c
         socket-factory-proxy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a Net-full-details is (list/c Address-family Transfer-family Address-list Net-permissions)

;; an Address-family is a Number

;; a Transfer-family is a Number

;; an Address-list is  a (listof (list/c (list/c Number Number) (listof (list/c Number Number))))

;; a Net-permissions is a (listof Net-permission)

;; a Net-permission is one of:
;; -- 'read
;; -- 'right
;; -- 'stat
;; -- 'connect
;; -- 'bind
;; -- 'listen
;; -- 'accept

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct socket-factory ([null #:mutable]))

(define (make-socket-factory) (socket-factory '()))

;; a Socket-factory-poxy is (socket-factory-proxy Net-full-details Param[Socket-factory-proxy]) 
(struct socket-factory-proxy (full-details param)
  #:property prop:contract
  (build-contract-property
   #:name
   (λ (ctc) 'socket-factory/c)
   #:first-order
   (λ (ctc) (λ (val) (socket-factory? val)))
   #:projection 
   (λ (ctc)
     (define full-details (socket-factory-proxy-full-details ctc))
     (define new-param (socket-factory-proxy-param ctc))
     (λ (blame)
       (λ (val)
         (unless (contract-first-order-passes? ctc val)
           (raise-blame-error blame val '(expected "a socket-factory" given "~e") val))
         (define old-rights (and (rights? val) (get-rights val)))
         (define rights (check-compatible compatible? full-details old-rights blame val "socket-factory")) 
         (impersonate-struct val 
                             socket-factory-null
                             (lambda (x v) v)
                             set-socket-factory-null!
                             (lambda (x v) v)
                             prop:rights
                             rights
                             prop:parameterized
                             new-param))))))


(define (socket-factory/c 
         #:address-family [af #f]
         #:transfer-family [tf #f]
         #:permissions [p empty])
  (socket-factory-proxy (list af tf empty p) #f))
  
(define (compatible? new-details old-details)
  (and (compatible-address-family (first new-details) (first old-details))
       (compatible-transfer-family (second new-details) (second old-details))
       (compatible-address-list (third new-details) (third old-details))
       (compatible-perms (fourth new-details) (fourth old-details))))

(define (compatible-address-family new-address-family old-address-family)
 (and old-address-family (= old-address-family new-address-family)))

(define (compatible-perms new-perms old-perms)
  (andmap (λ (x) (member x old-perms)) new-perms))

(define (compatible-transfer-family new-transfer-family old-transfer-family)
 (and old-transfer-family (= old-transfer-family new-transfer-family)))

(define (compatible-address-list new-address-list old-address-list)
  (define (sub-range? new-range)
    (λ (old-range)
      (and (list? new-range)
           (= (length new-range) 2)
           (andmap number? new-range)
           (<= (first new-range) (second new-range))
           (>= (first new-range) (first old-range))
           (<= (second new-range) (second old-range)))))
  (define (sub-address? new-address)
    (λ (old-address)
      (define new-just-ips (first new-address))
      (define old-just-ips (first old-address))
      (define new-just-ports (second new-address))
      (define old-just-ports (second old-address))
      (and ((sub-range? new-just-ips) old-just-ips)
           (andmap (within sub-range? old-just-ports) new-just-ports))))
  (define (within what? old-ranges)
    (λ (new-range) (ormap (what? new-range) old-ranges)))
  (andmap (within sub-address? old-address-list) new-address-list))
