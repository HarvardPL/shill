#lang racket

(require rackunit rackunit/text-ui shill/private/contract-utils "../test-utils.rkt")


(define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))

(test-pass
 "Successfull call of higher-order parametric function"
 (script 
  (module cap racket
    (require shill/private/filesystem shill/private/contract-utils)
    (provide 
     (contract-out [f 
                    (forall/c ([X (list (list "path" #t) (list "lookup" #t))]) 
                              (-> (-> X (listof string?)) X (listof string?)))]))    
    (define (f g x) (g x)))
  (module ambient racket
   (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define child-dir (create-dir tmpdir "foo"))    
    (f contents tmpdir))
  (require 'ambient)))


(test-pass
 "Successfull call of higher-order parametric function after a legitimate path"
 (script 
  (module cap racket
    (require shill/private/filesystem shill/private/contract-utils)
    (provide 
     (contract-out [f 
                    (forall/c ([X (list (list "path" #t) (list "lookup" #t))]) 
                              (-> (-> X (listof string?)) X (listof string?)))]))    
    (define (f g x) 
      (let* ([result1 (path x)]
             [result2 (g x)])
        result2)))
  (module ambient racket
   (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define child-dir (create-dir tmpdir "foo"))    
    (f contents tmpdir))
  (require 'ambient)))



(test-pass
 "Successfull call of higher-order parametric function after a legitimate lookup and path on its reult"
 (script 
  (module cap racket
    (require shill/private/filesystem shill/private/contract-utils)
    (provide 
     (contract-out [f 
                    (forall/c ([X (list (list "path" #t) (list "lookup" #t))]) 
                              (-> (-> X (listof string?)) X (listof string?)))]))    
    (define (f g x) 
      (let* ([result1 (lookup x "foo")] 
             [result2 (path result1)]
             [result3 (g result1)])
        result3)))
  (module ambient racket
   (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define child-dir (create-dir tmpdir "foo"))    
    (f contents tmpdir))
  (require 'ambient)))

(test-contract-fail
 "Failed call to parametric function (passing non parmetrized argument)"
 (script 
  (module cap racket
    (require shill/private/filesystem shill/private/contract-utils)
    (provide 
     (contract-out [f 
                    (forall/c ([X (list (list "path" #t) (list "lookup" #t))]) 
                              (-> (-> X (listof string?)) X  (dir/c #:path (list "path" #t)) (listof string?)))]))    
    (define (f g x y) (g y)))
  (module ambient racket
   (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define child-dir (create-dir tmpdir "foo"))    
    (f contents tmpdir tmpdir))
  (require 'ambient))
 "cap")

(test-contract-fail
 "Failed contents on parametrized directory"
 (script 
  (module cap racket
    (require shill/private/filesystem shill/private/contract-utils)
    (provide 
     (contract-out [f 
                    (forall/c ([X (list (list "path" #t) (list "lookup" #t))]) 
                              (-> (-> X (listof string?)) X (listof string?)))]))    
    (define (f g x) 
      (let* ([result1 (contents x)]
             [result2 (g x)])
        result2)))
  (module ambient racket
   (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define child-dir (create-dir tmpdir "foo"))    
    (f contents tmpdir))
  (require 'ambient))
 "cap")


(test-contract-fail
 "Failed contents on the result of lookup on parametric directory"
 (script 
  (module cap racket
    (require shill/private/filesystem shill/private/contract-utils)
    (provide 
     (contract-out [f 
                    (forall/c ([X (list (list "path" #t) (list "lookup" #t))]) 
                              (-> (-> X (listof string?)) X (listof string?)))]))    
    (define (f g x) 
      (let* ([result1 (lookup x "foo")] 
             [result2 (contents result1)]
             [result3 (g result1)])
        result3)))
  (module ambient racket
   (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define child-dir (create-dir tmpdir "foo"))    
    (f contents tmpdir))
  (require 'ambient))
 "cap")
