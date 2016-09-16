
#lang racket

(provide (rename-out [shill-dir? dir?]
                     [shill-file? file?]
                     [shill-pipe-factory? pipe-factory?])
         cap?
         sys-error?
         pipe-end?
         dir/c
         file/c
         pipe-factory/c
         pipe-end/c
         open-dir
         open-file
         cwd
         stdin
         stdout
         stderr
         pipe-factory
         path
         contents
         create-dir
         create-file
         create-pipe
         add-link
         add-symlink
         read-symlink
	 unlink-symlink
         unlink-file
         unlink-dir
         rename
         lookup
         chdir
         read
         write
         append
         exec
         dir-proxy
         file-proxy
         pipe-end-proxy
         pipe-factory-proxy
         close
	 size
	 created
	 accessed
	 modified
	 status-changed
	 uid suid
	 gid sgid
	 perms
	 (struct-out stat))

(require (only-in racket/base [append list-append])
         racket/contract
         shill/private/net
         shill/private/utils
         (rename-in shill/private/ffi 
                    [open-dir rt:open-dir]
                    [open-file rt:open-file]
                    [open-for-exec rt:open-for-exec]
                    [close-node rt:close-node]
                    [contents rt:contents]
                    [lookup-file rt:lookup-file]
                    [lookup-for-exec rt:lookup-for-exec]
                    [lookup-dir rt:lookup-dir]
                    [read-link rt:read-link]
                    [add-link rt:add-link]
                    [add-symlink rt:add-symlink]
                    [unlink-file rt:unlink-file]
		    [unlink-symlink rt:unlink-symlink]
                    [unlink-dir rt:unlink-dir]
                    [rename-node rt:rename]
                    [create-file rt:create-file]
                    [create-dir rt:create-dir]
                    [read-file rt:read-file]
                    [append-file rt:append-file]
                    [write-file rt:write-file]
                    [new-pipe rt:create-pipe]
                    [read-pipe rt:read-pipe]
                    [write-pipe rt:write-pipe]
                    [path rt:path]
                    [stdin rt:stdin]
                    [stdout rt:stdout]
                    [stderr rt:stderr]
                    [cwd rt:cwd]
                    [chdir rt:chdir]
		    [shill-stat rt:stat])
         (rename-in "sandbox.rkt"
                    [shill-sandbox rt:sandbox]))


(struct shill-dir (desc
                   [path #:mutable]
                   [contents #:mutable]
                   [create-dir #:mutable]
                   [create-file #:mutable]
                   [add-link #:mutable]
                   [add-symlink #:mutable]
                   [read-symlink #:mutable]
                   [unlink-file #:mutable]
		   [unlink-symlink #:mutable]
                   [unlink-dir #:mutable]
                   [rename #:mutable]
                   [lookup #:mutable]
                   [chdir #:mutable]
                   [close #:mutable]
		   [stat #:mutable]))

(struct shill-file (desc
                    type
                    [path #:mutable]
                    [read #:mutable]
                    [append #:mutable]
                    [write #:mutable]
                    [exec #:mutable]
                    [close #:mutable]
		    [stat #:mutable]))

(struct shill-pipe-factory ([create #:mutable]))

(define (build-dir dir p)
  
  (define path-cache p)
  
  (define (path)
    (match (rt:path dir)
      [#f path-cache]
      [p (let ([as-str (path->string p)])
           (set! path-cache as-str)
           as-str)]))
  
  (define (contents)
    (rt:contents dir))
  
  (define (create-dir name) 
    (let ([p (string-append (path) "/" name)])
      (build-dir (rt:create-dir dir (string->path-element name)) p)))
  
  (define (create-file name)
    (let ([p (string-append (path) "/" name)])
      (build-file (rt:create-file dir (string->path-element name)) p)))
  
  (define (add-link name target) (rt:add-link dir (string->path-element name) (shill-file-desc target)))
  
  (define (add-symlink name target) (rt:add-symlink dir (string->path-element name) (string->path target)))
  
  (define (read-symlink name)
    (let ([result (rt:read-link dir (string->path-element name))])
      (if (path? result)
          (path->string result)
          result)))
  
  (define (unlink-file target target-name) 
    (define name (string->path-element target-name))
    (define node (shill-file-desc target))
    (rt:unlink-file dir name node))
  
  (define (unlink-symlink target-name)
    (define name (string->path-element target-name))
    (rt:unlink-symlink dir name))

  (define (unlink-dir target target-name) 
    (define name (string->path-element target-name))
    (define node (shill-dir-desc target))
    (rt:unlink-dir dir name node))
  
  (define (rename target old-name target-dir new-name)
    (rt:rename dir (string->path-element old-name) (shill-dir-desc target-dir) (string->path-element new-name)))
  
  (define (lookup target-name mode)
    (define p (string-append (path) "/" target-name))
    (define name (string->path-element target-name))
    (if mode 
        (let ([maybe-fd-exec (rt:lookup-for-exec dir name)])
          (cond [(sys-error? maybe-fd-exec) maybe-fd-exec]
                [else (build-file maybe-fd-exec p)]))
        (let ([maybe-fd (rt:lookup-dir dir name)])
          (cond [(sys-error? maybe-fd) 
                 (let ([maybe-fd (rt:lookup-file dir name)])
                   (cond [(sys-error? maybe-fd) maybe-fd]
                         [else (build-file maybe-fd p)]))]
                [else (build-dir maybe-fd p)]))))
  
  (define (chdir)
    (rt:chdir dir))
  
  (define (close)
    (rt:close-node dir))

  (define (stat)
    (rt:stat dir))
  
  (shill-dir dir path contents create-dir create-file  add-link add-symlink read-symlink
	     unlink-file unlink-symlink unlink-dir rename lookup chdir close stat))

(define (build-file file p)
  
  (define path-cache p)
  
  (define (path)
    (match (rt:path file)
      [#f path-cache]
      [p (let ([as-str (path->string p)])
           (set! path-cache as-str)
           as-str)]))
  
  (define (read) (rt:read-file file))
  
  (define (append input) (rt:append-file file input))
  
  (define (write input) (rt:write-file file input))
  
  (define (exec file-rights args stdin stdout stderr caps time)
    (define (arg-to-string a)
      (cond
        [(string? a) a]
        [(shill-dir? a) ((shill-dir-path a))]
        [(shill-file? a) ((shill-file-path a))]))
    (define (std-to-desc s)
      (cond
        [(shill-file? s) (shill-file-desc s)]
        [else #f]))
    (define (cap-to-desc c)
      (cond
        [(shill-file? c) (shill-file-desc c)]
        [(shill-dir? c) (shill-dir-desc c)]))
    
    (define shill-node? (λ (a) (or (shill-dir? a) (shill-file? a))))
    
    (define strargs (map arg-to-string args))
    (define nodeargs (filter shill-node? args))
    (define stdin-desc (std-to-desc stdin))
    (define stdout-desc (std-to-desc stdout))
    (define stderr-desc (std-to-desc stderr))
    (define all-caps (list-append (filter (λ (e) e) (list stdin stdout stderr))
                                  nodeargs caps))
    ;; Permissions = [Listof [U 'read 'write 'stat 'connect 'bind 'listen 'accept]]
    ;; socket-factory-rights : [Listof [List Number
    ;;                                       Number
    ;;                                       [Listof [List [List Number Number]]]
    ;;                                       Permissions]]
    (define-values (node-rights pipe-factory-rights socket-factory-rights)
      (for/fold ([node-rights empty]
                 [pipe-factory-rights empty]
                 [socket-factory-rights empty])
        ([cap all-caps])
        (cond [(or (shill-dir? cap) (shill-file? cap)) 
               (values (cons (pick-node-rights cap) node-rights)
                       pipe-factory-rights
                       socket-factory-rights)]
              [(shill-pipe-factory? cap)
               (values node-rights
                       (cons (pick-pipe-factory-rights cap) pipe-factory-rights)
                       socket-factory-rights)]
              [(socket-factory? cap)
               (values node-rights
                       pipe-factory-rights
                       (cons (pick-socket-factory-rights cap) socket-factory-rights))])))
    (define files (cons file (filter-map (λ (x) (and (shill-node? x) (cap-to-desc x))) all-caps)))
    (define rights (cons file-rights (reverse node-rights)))
    (rt:sandbox file stdin-desc stdout-desc stderr-desc files rights pipe-factory-rights socket-factory-rights strargs time))
  
  (define (close)
    (rt:close-node file))
  
  (define (stat)
    (rt:stat file))

  (shill-file file 'file path read append write exec close stat))


(define (build-pipe-end pipe-end)
  
  (define (read) (rt:read-pipe pipe-end))
  
  (define (write input) (rt:write-pipe pipe-end input))
  
  (define (throw-error . args) 
    (error "operation not supported on pipe ends"))
  
  (define (close)
    (rt:close-node pipe-end))

  (define (stat)
    (rt:stat pipe-end))
  
  (shill-file pipe-end 'pipe-end throw-error read write write throw-error close stat))

(define (path-element-string? s)
  (and (path-string? s) (andmap (λ (c) (not (char=? c #\/))) (string->list s))))


(define (pipe-end-compatible? rights)
  (andmap (λ (x) (member x (list 'read 'write 'stat))) rights))

(define (node-compatible? new-rights old-rights)
  (define (find key l) 
    (findf 
     (λ (x) 
       (cond [(cons? x) (symbol=? (first x) key)]
             [else (symbol=? x key)]))
       l))
  (define (compare old-rights)
    (λ (new-right)
      (let* ([new-right-symbol? (symbol? new-right)]
             [new-right-key (if new-right-symbol? new-right (first new-right))]
             [old-right (find new-right-key old-rights)])        
        (or (and new-right-symbol? (symbol? old-right))
            (and (list? old-right)
                 (let ([nested-new-rights (second new-right)]
                       [nested-old-rights (second old-right)])
                   (cond 
                     [(and nested-new-rights nested-old-rights)
                      (node-compatible? nested-new-rights nested-old-rights)]
                     [nested-new-rights (node-compatible? nested-new-rights old-rights)]
                     [nested-old-rights (node-compatible? new-rights nested-old-rights)]
                     [else  #t])))))))
    (andmap (compare old-rights) new-rights))


(define (pipe-factory-compatible? new-rights old-rights)
  (and (node-compatible? (first new-rights) (first old-rights))
       (node-compatible? (second new-rights) (second old-rights))))



(define (make-rights details)
  (define (shape-right right-name #:complex? [complex? #f])
    (let ([maybe-complex (member right-name '(lookup create-file create-dir))])
      (cond [(and maybe-complex complex? (list? complex?))
             (list right-name (make-rights complex?))]
            [maybe-complex (list right-name #f)]
            [else right-name])))
  (define (remove-clatter detail)
    (let ([dl (length detail)]
          [right-name (string->symbol (first detail))])
      (or (and (= dl 2) (not (false? (second detail))) (shape-right right-name))
          (and (= dl 3) (shape-right right-name #:complex? (third detail))))))
  (foldl 
   (λ (head tail) 
     (let ([new (remove-clatter head)])
       (if  new (cons new tail) tail)))
   empty
   details))

(define (pick-node-rights node)
  (cond [(rights? node) 
         (let ([rights (get-rights node)])
           (if (procedure? rights) (rights) rights))]
        [else (list 'path 'contents (list 'create-dir #f) (list 'create-file #f) 'add-link 'add-symlink
                    'read-symlink 'unlink-file 'unlink-symlink 'unlink-dir (list 'lookup #f)
                    'link 'read 'write 'append 'exec 'stat 'close 'chroot 'chdir 'chflags 'chmod
                    'chown 'chtimes 'unlink 'rename 'read-extattr)]))

(define (pick-pipe-factory-rights factory)
  (cond [(rights? factory) 
         (let* ([rights (get-rights factory)])
           (cond [(procedure? rights) (rights)]
                 [else (list (first rights) (second rights))]))]
        [else (let ([default-pipe-end-rights '(read write append stat close)])
                (list default-pipe-end-rights default-pipe-end-rights))]))

(define (pick-socket-factory-rights factory)
  (cond [(rights? factory)
         (let ([rights (get-rights factory)])
           (if (procedure? rights) (rights) rights))]
        [else empty]))


(struct maybe-protect/c (ctc-fun maybe-param)
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
     (define maybe-param (maybe-protect/c-maybe-param ctc))
     (define ctc-fun (maybe-protect/c-ctc-fun ctc))
     (if (boolean? maybe-param)
         (λ (blame)
           (λ (val)
             (((contract-projection (ctc-fun maybe-param)) blame) val))) 
         (let ([make (param-make maybe-param)])
           (λ (blame)
             (λ (val)
               (((contract-projection (ctc-fun (param make (make val)))) blame) val)))))))) 


(define (make-path/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->* () #:pre (second details) (or/c sys-error? path-string?))]
                [else
                 (->* () #:pre (second details) (or/c sys-error? (and path-string? (third details))))])]
         [else (->* () #:pre #f any)])
   "path"))

(define (make-stat/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->* () #:pre (second details) (or/c sys-error? stat?))])]
         [else (->* () #:pre #f any)])
   "path"))

(define (make-chdir/c details)
  (enhance-blame/c
   (cond [(list? details)
          (->* () #:pre (second details) (or/c sys-error? void?))]
         [else (->* () #:pre #f any)])
   "chdir"))

(define (make-close/c details)
  (enhance-blame/c
   (cond [(list? details) (->* () #:pre (second details) any)]
         [else (->* () #:pre #f any)])
   "close"))

(define (make-contents/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->* () #:pre (second details) (or/c sys-error? (listof path-element-string?)))]
                [else
                 (->* () #:pre (second details) (or/c sys-error? (filter/c (third details))))])]
         [else (->* () #:pre #f any)])
   "contents"))


(define (make-create-dir/c details full-details old-param)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2) 
                 (->i ([path-to-lookup path-element-string?]) 
                      #:pre () (second details) 
                      [result (maybe-protect/c (λ (new-param) (or/c sys-error? (dir-proxy full-details new-param))) old-param)])]
                [(and (= dl 3) (list? (third details)))
                 (->i ([path-to-lookup path-element-string?])
                      #:pre () (second details) 
                      [result (maybe-protect/c (λ (new-param) (or/c sys-error? (dir-proxy (third details) new-param))) old-param)])]
                [(= dl 3)
                 (->i ([path-to-lookup (and/c path-element-string? (third details))])
                      #:pre () (second details) 
                      [result (maybe-protect/c (λ (new-param) (or/c sys-error? (dir-proxy full-details new-param))) old-param)])]
                [else 
                 (->i ([path-to-lookup (and/c path-element-string? (fourth details))])
                      #:pre () (second details)
                      [result  (maybe-protect/c (λ (new-param) (or/c sys-error? (dir-proxy (third details) new-param))) old-param)])])]
         [else (->* (any/c) #:pre #f any)])
   "create-dir"))


(define (make-create-file/c details full-details old-param)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2) 
                 (->i ([name-of-new-file path-element-string?]) 
                      #:pre () (second details)
                      [result
                       (maybe-protect/c 
                        (λ (new-param) (or/c sys-error? (file-proxy (dir-details->file-details full-details) new-param))) 
                        old-param)])]
                [(and (= dl 3) (list? (third details)))
                 (->i ([name-of-new-file path-element-string?])
                      #:pre () (second details)
                      [result 
                       (maybe-protect/c
                        (λ (new-param) (or/c sys-error? (file-proxy (dir-details->file-details (third details)) new-param)))
                        old-param)])]
                [(= dl 3)
                 (->i ([name-of-new-file (and/c path-element-string? (third details))])
                      #:pre () (second details)
                      [result
                       (maybe-protect/c
                        (λ (new-param) (or/c sys-error? (file-proxy (dir-details->file-details full-details) new-param)))
                        old-param)])]
                [else 
                 (->i ([name-of-new-file (and/c path-element-string? (fourth details))])
                      #:pre () (second details)
                      [result
                       (maybe-protect/c
                        (λ (new-param) (or/c sys-error? (file-proxy (dir-details->file-details (third details)) new-param)))
                        old-param)])])]
         [else (->* (any/c) #:pre #f any)])
   "create-file/c"))



(define (make-add-link/c details)
  (define details-mask (list (list "link" #t)))
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([name-of-new-link path-element-string?]
                       [target-file (file-proxy details-mask #f)])
                      #:pre () (second details)
                      any)]
                [else 
                 (->i ([name-of-new-link (and/c path-element-string? (third details))]
                       [target-file (file-proxy details-mask #f)])
                      #:pre () (second details) 
                      any)])]
         [else (->* (any/c any/c) #:pre #f any)])
   "add-link"))

(define (make-add-symlink/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([name-of-new-symlink path-element-string?]
                       [path-of-target path-string?])
                      #:pre () (second details)
                      any)]
                [(= dl 3) 
                 (->i ([name-of-new-symlink (and/c (third details) path-element-string?)]
                       [path-of-target path-string?])
                      #:pre () (second details)
                      any)]
                [else 
                 (->i ([name-of-new-symlink (and/c (third details) path-element-string?)]
                       [path-of-target (and/c (fourth details) path-string?)]) 
                      #:pre () (second details)
                      any)])]
         [else (->* (any/c any/c) #:pre #f any)])
   "add-symlink"))

(define (make-read-symlink/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([name-of-symlink path-element-string?]) 
                      #:pre () (second details)
                      [result (or/c sys-error? path-string?)])]
                [(= dl 3) 
                 (->i ([name-of-symlink (and/c path-element-string? (third details))])
                      #:pre () (second details)
                      [result (or/c sys-error? path-string?)])]
                [else 
                 (->i ([name-of-symlink (and/c path-element-string? (third details))])
                      #:pre () (second details)
                      [result (or/c sys-error? (and/c path-string? (fourth details)))])])]
         [else (->* (any/c) #:pre #f any)])
   "read-symlink"))

(define (make-unlink/c details)
  (define details-mask (list (list "unlink" #t)))
  (enhance-blame/c
   (cond ([list? details]
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([target (or/c (dir-proxy details-mask #f) (file-proxy details-mask #f))]
                       [name-of-target path-element-string?]) 
                      #:pre () (second details)
                      any)]
                [else 
                 (->i ([target (or/c (dir-proxy details-mask #f) (file-proxy details-mask #f))]
                       [name-of-target (and/c path-element-string? (third details))]) 
                      #:pre () (second details)
                      any)]))
         (else (->* (any/c any/c) #:pre #f any)))
   "unlink"))

(define (make-unlink-symlink/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([name-of-symlink path-element-string?]) 
                      #:pre () (second details)
                      any)]
                [else 
                 (->i ([name-of-symlink (and/c path-element-string? (third details))])
                      #:pre () (second details)
                      any)])]
         [else (->* (any/c) #:pre #f any)])
   "unlink-symlink"))

(define (make-rename/c details)
  (define details-mask (list (list "rename" #t)))
  (define (rename-to? dir)
    (if (shadow-details? dir)
        (let ([sds (get-shadow-details dir)])
          (and (list? sds) (second sds)))
        #t))
  (define (rename-to-details dir)
    (let ([sds (get-shadow-details dir)])
      (if (and (list? sds) (= (length sds) 3))
          (third sds)
          any/c)))
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([target (or/c (dir-proxy details-mask #f) (file-proxy details-mask #f))]
                       [old-name-of-target path-element-string?]
                       [destination-directory rename-to?]
                       [new-name-for-target (destination-directory) (and/c path-element-string?
                                                                           (rename-to-details destination-directory))])
                      #:pre () (second details)
                      any)]
                [else 
                 (->i ([target (or/c (dir-proxy details-mask #f) (file-proxy details-mask #f))]
                       [old-name-of-target (and/c path-element-string? (third details))] 
                       [destination-directory rename-to?] 
                       [new-name-for-target (destination-directory) (and/c path-element-string? (rename-to-details destination-directory))]) 
                      #:pre () (second details) 
                      any)])]
         [else (->* (any/c any/c any/c any/c) #:pre #f any)])
   "rename"))

(define (make-lookup/c details full-details old-param)
  (enhance-blame/c
   (cond [(list? details)
          (define default-file-details (dir-details->file-details full-details))
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([name-of-target path-element-string?] 
                       [lookup-mode (or/c false? (λ (s) (and (string? s) (string=? s "exec"))))]) 
                      #:pre () (second details)
                      [result
                       (maybe-protect/c
                        (λ (new-param)
                          (or/c
                           sys-error?
                           (dir-proxy full-details new-param)
                           (file-proxy default-file-details new-param)))
                        old-param)])]
                [(and (= dl 3) (list? (third details))) 
                 (->i ([name-of-target path-element-string?]
                       [lookup-mode (or/c false? (λ (s) (and (string? s) (string=? s "exec"))))])
                      #:pre () (second details)
                      [result
                       (maybe-protect/c
                        (λ (new-param)
                          (or/c sys-error?
                                (dir-proxy (third details) new-param)
                                (file-proxy (dir-details->file-details (third details)) new-param)))
                        old-param)])]
                [(= dl 3) 
                 (->i ([name-of-target (and/c path-element-string? (third details))]
                       [lookup-mode (or/c false? (λ (s) (and (string? s) (string=? s "exec"))))])
                      #:pre () (second details)
                      [result
                       (maybe-protect/c
                        (λ (new-param)
                          (or/c sys-error?
                                (dir-proxy full-details new-param)
                                (file-proxy (dir-details->file-details full-details) new-param)))
                        old-param)])]
                [else
                 (->i ([name-of-target (and/c path-element-string? (fourth details))]
                       [lookup-mode (or/c false? (λ (s) (and (string? s) (string=? s "exec"))))])
                      #:pre () (second details)
                      [result
                       (maybe-protect/c
                        (λ (new-param)
                          (or/c sys-error?
                                (dir-proxy (third details) new-param)
                                (file-proxy (dir-details->file-details (third details)) new-param)))
                        old-param)])])]
         [else (->* (any/c any/c) #:pre #f any)])
   "lookup"))

(define (make-read/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->* () #:pre (second details) (or/c sys-error? bytes?))]
                [else 
                 (->* () #:pre (second details) (or/c sys-error? (and/c bytes? (third details))))])]
         [else (->* () #:pre #f any)])
   "read"))

(define (make-append/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([bytes-to-append bytes?]) #:pre () (second details) any)]
                [else 
                 (->i ([bytes-to-append (and/c bytes? (third details))]) #:pre () (second details) any)])]
         [else (->* (any/c) #:pre #f any)])
   "append"))

(define (make-write/c details)
  (enhance-blame/c
   (cond [(list? details)
          (define dl (length details))
          (cond [(= dl 2)
                 (->i ([bytes-to-write bytes?]) #:pre () (second details) any)]
                [else 
                 (->i ([bytes-to-write (and/c bytes? (third details))]) #:pre () (second details) any)])]
         [else (->* (any/c) #:pre #f any)])
   "write"))

(define (make-exec/c details)
  (enhance-blame/c
   (cond [(list? details)
          (->i ([executable-privileges any/c]
                [arguments-of-the-executable (listof (or/c shill-file? shill-dir? string?))]
                [stdin (or/c false? shill-file?)]
                [stdout (or/c false? shill-file?)]
                [stderr (or/c false? shill-file?)]
                [extra-capabilities (listof cap?)]
                [time (or/c false? (and/c fixnum? positive?))])
               #:pre () (second details)
               any)]
         [else (->* (any/c any/c any/c any/c any/c any/c) #:pre #f any)])
   "exec"))

(define (make-create-pipe/c first-details second-details old-param)
  (-> (maybe-protect/c 
       (λ (new-param) 
         (list/c (pipe-end-proxy first-details new-param)
                 (pipe-end-proxy second-details new-param)))
       old-param)))


(define (file-details-add-defaults details)
  (define (assoc-or-false key details)
    (let ([val (assoc key details)])
      (cond
        [(false? val) (list key #f)]
        [else val])))
  (map (λ (key) (assoc-or-false key details))
       (list "path" "read" "write" "append" "link" "exec" "stat" "close" "rename" "unlink" "read-extattr" "write-extattr")))




(define (dir-details->file-details details)
  (filter list? 
          (list
           (assoc "path" details)
           (assoc "read" details)
           (assoc "write" details)
           (assoc "append" details)
           (assoc "link" details)
           (assoc "exec" details)
           (assoc "stat" details)
           (assoc "close" details)
           (assoc "rename" details)
           (assoc "unlink" details)
           (assoc "read-extattr" details)
           (assoc "write-extattr" details))))

(struct filter/c (pred)
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         (if (sys-error? val) 
             val
             (filter
              (filter/c-pred ctc)
              (((contract-projection (listof string?)) blame) val))))))))


(struct dir-proxy (full-details param) 
  #:property prop:contract 
  (build-contract-property
   #:name
   (λ (ctc) 'dir/c)
   #:first-order
   (λ (ctc) 
     (λ (val)
       (shill-dir? val)))
   #:projection 
   (λ (ctc)
     (define full-details (dir-proxy-full-details ctc))
     (define new-rights (make-rights full-details))
     (define new-param (dir-proxy-param ctc))
     (define path/c (make-path/c (assoc "path" full-details)))
     (define close/c (make-close/c (assoc "close" full-details)))
     (define contents/c (make-contents/c (assoc "contents" full-details)))
     (define create-dir/c 
       (make-create-dir/c (assoc "create-dir" full-details) full-details new-param))
     (define create-file/c
       (make-create-file/c (assoc "create-file" full-details) full-details new-param))
     (define add-link/c (make-add-link/c (assoc "add-link" full-details)))
     (define add-symlink/c (make-add-symlink/c (assoc "add-symlink" full-details)))
     (define read-symlink/c (make-read-symlink/c (assoc "read-symlink" full-details)))
     (define unlink-file/c (make-unlink/c (assoc "unlink-file" full-details)))
     (define unlink-symlink/c (make-unlink-symlink/c (assoc "unlink-symlink" full-details)))
     (define unlink-dir/c (make-unlink/c (assoc "unlink-dir" full-details)))
     (define rename/c (make-rename/c (assoc "rename-from" full-details)))
     (define lookup/c 
       (make-lookup/c (assoc "lookup" full-details) full-details new-param))
     (define chdir/c
       (make-chdir/c (assoc "chdir" full-details)))
     (define stat/c
       (make-stat/c (assoc "stat" full-details)))
     (define shadow-details (assoc "rename-to" full-details))
     (λ (blame)
       (define (redirect-proc accessor-contract)
         (λ (dir field-value)
           (((contract-projection accessor-contract) blame) field-value)))
       (λ (val)
         (unless (contract-first-order-passes? ctc val)
           (raise-blame-error blame val '(expected "a directory" given "~e") val))
         (define old-rights (and (rights? val) (get-rights val)))
         (define rights (check-compatible node-compatible? new-rights old-rights blame val "directory"))
         (impersonate-struct val 
                             shill-dir-path
                             (redirect-proc path/c)
                             shill-dir-contents 
                             (redirect-proc contents/c)
                             shill-dir-create-file
                             (redirect-proc create-file/c)
                             shill-dir-create-dir
                             (redirect-proc create-dir/c)
                             shill-dir-add-link
                             (redirect-proc add-link/c)
                             shill-dir-add-symlink
                             (redirect-proc add-symlink/c)
                             shill-dir-read-symlink
                             (redirect-proc read-symlink/c)
                             shill-dir-unlink-file
                             (redirect-proc unlink-file/c)
                             shill-dir-unlink-symlink
                             (redirect-proc unlink-symlink/c)
                             shill-dir-unlink-dir
                             (redirect-proc unlink-dir/c)
                             shill-dir-rename
                             (redirect-proc rename/c)
                             shill-dir-lookup
                             (redirect-proc lookup/c)
                             shill-dir-chdir
                             (redirect-proc chdir/c)
                             shill-dir-close
                             (redirect-proc close/c)
			     shill-dir-stat
			     (redirect-proc stat/c)
                             set-shill-dir-path!
                             mutator-redirect-proc
                             set-shill-dir-contents! 
                             mutator-redirect-proc
                             set-shill-dir-create-file!
                             mutator-redirect-proc
                             set-shill-dir-create-dir!
                             mutator-redirect-proc
                             set-shill-dir-add-link!
                             mutator-redirect-proc
                             set-shill-dir-add-symlink!
                             mutator-redirect-proc
                             set-shill-dir-read-symlink!
                             mutator-redirect-proc
                             set-shill-dir-unlink-file!
                             mutator-redirect-proc
                             set-shill-dir-unlink-symlink!
                             mutator-redirect-proc
                             set-shill-dir-unlink-dir!
                             mutator-redirect-proc
                             set-shill-dir-rename!
                             mutator-redirect-proc
                             set-shill-dir-lookup!
                             mutator-redirect-proc
                             set-shill-dir-chdir!
                             mutator-redirect-proc
                             set-shill-dir-close!
                             mutator-redirect-proc
			     set-shill-dir-stat!
			     mutator-redirect-proc
                             prop:rights
                             rights
                             prop:parameterized
                             new-param
                             prop:shadow-details
                             shadow-details))))))


(struct file-proxy (full-details param) 
  #:property prop:contract 
  (build-contract-property
   #:name
   (λ (ctc) 'file/c)
   #:first-order
   (λ (ctc)
     (λ (val)
       (and (shill-file? val))))
   #:projection 
   (λ (ctc)
     (define full-details (file-proxy-full-details ctc))
     (define new-rights (make-rights full-details))
     (define new-param (file-proxy-param ctc))
     (define close/c (make-close/c (assoc "close" full-details)))
     (define path/c (make-path/c (assoc "path" full-details)))
     (define read/c (make-read/c (assoc "read" full-details)))
     (define write/c (make-write/c (assoc "write" full-details)))
     (define append/c (make-append/c (assoc "append" full-details)))
     (define stat/c (make-stat/c (assoc "stat" full-details)))
     (define exec/c (make-exec/c (assoc "exec" full-details)))
     (λ (blame)
       (define (redirect-proc accessor-contract)
         (λ (file field-value)
           (((contract-projection accessor-contract) blame) field-value)))
       (λ (val)
         (unless (contract-first-order-passes? ctc val)
           (raise-blame-error blame val '(expected "a file or pipe-end" given "~e") val))
         (define old-rights (and (rights? val) (get-rights val)))
         (define rights (check-compatible node-compatible? new-rights old-rights blame val "file"))
         (impersonate-struct val
                             shill-file-path
                             (redirect-proc path/c)
                             shill-file-read 
                             (redirect-proc read/c)
                             shill-file-append
                             (redirect-proc append/c)
                             shill-file-write
                             (redirect-proc write/c)
                             shill-file-exec
                             (redirect-proc exec/c)
                             shill-file-close
                             (redirect-proc close/c)
			     shill-file-stat
			     (redirect-proc stat/c)
                             set-shill-file-path!
                             mutator-redirect-proc
                             set-shill-file-read! 
                             mutator-redirect-proc
                             set-shill-file-append!
                             mutator-redirect-proc
                             set-shill-file-write!
                             mutator-redirect-proc
                             set-shill-file-exec!
                             mutator-redirect-proc
                             set-shill-file-close!
                             mutator-redirect-proc
			     set-shill-file-stat!
			     mutator-redirect-proc
                             prop:rights
                             rights
                             prop:parameterized
                             new-param))))))


(struct pipe-end-proxy (full-details param)
  #:property prop:contract 
  (build-contract-property
   #:name
   (λ (ctc) 'pipe/c)
   #:first-order
   (λ (ctc)
     (λ (val)
       (and (shill-file? val) (symbol=? (shill-file-type val) 'pipe-end))))
   #:projection 
   (λ (ctc)
     (define details (pipe-end-proxy-full-details ctc))
     (define param (pipe-end-proxy-param ctc))
     (λ (blame)
       (λ (val)
         (unless (contract-first-order-passes? ctc val)
           (raise-blame-error blame val '(expected "a pipe-end" given "~e") val))
         (((contract-projection (file-proxy details param)) blame) val))))))


(struct pipe-factory-proxy (full-details param)
  #:property prop:contract 
  (build-contract-property
   #:name
   (λ (ctc) 'pipe-factory/c)
   #:first-order
   (λ (ctc)
     (λ (val)
       (shill-pipe-factory? val)))
   #:projection 
   (λ (ctc)
     (define full-details (pipe-factory-proxy-full-details ctc))
     (define new-rights (map make-rights full-details))
     (define new-param (pipe-factory-proxy-param ctc))
     (define create-pipe/c (make-create-pipe/c (first full-details) (second full-details) new-param))
     (λ (blame)
       (define (redirect-proc accessor-contract)
         (λ (dir field-value)
           (((contract-projection accessor-contract) blame) field-value)))
       (λ (val)
         (unless (contract-first-order-passes? ctc val)
           (raise-blame-error blame val '(expected "a pipe-factory" given "~e") val))
         (define old-rights (and (rights? val) (get-rights val)))
         (define rights (check-compatible pipe-factory-compatible? new-rights old-rights blame val "pipe-factory"))
         (impersonate-struct val 
                             shill-pipe-factory-create
                             (redirect-proc create-pipe/c)
                             set-shill-pipe-factory-create!
                             mutator-redirect-proc
                             prop:rights
                             rights
                             prop:parameterized
                             new-param))))))

(define (dir/c
         #:path [p (list "path" #f)]
         #:contents [c (list "contents" #f)]
         #:create-dir [crd (list "create-dir" #f)]
         #:create-file [crf (list "create-file" #f)]
         #:add-link [al (list "add-link" #f)]
         #:add-symlink [asl (list "add-symlink" #f)]
         #:read-symlink [rsl (list "read-symlink" #f)]
         #:unlink-file [uf (list "unlink-file" #f)]
         #:unlink-symlink [us (list "unlink-symlink" #f)]
         #:unlink-dir [ud (list "unlink-dir" #f)]
         #:rename-from [rnf (list "rename-from" #f)]
         #:rename-to [rnt (list "rename-to" #f)]
         #:lookup [l (list "lookup" #f)]
         #:link [lf (list "link" #f)]
         #:read [rf (list "read" #f)]
         #:write [wf (list "write" #f)]
         #:append [af (list "append" #f)]
         #:exec [x (list "exec" #f)]
         #:stat [s (list "stat" #f)]
         #:close [cl (list "close" #f)]
         #:chroot [chr (list "chroot" #f)]
         #:chdir [chd (list "chdir" #f)]
         #:chflags [chf (list "chflags" #f)]
         #:chmod [chm (list "chmod" #f)]
         #:chown [cho (list "chown" #f)]
         #:chtimes [cht (list "chtimes" #f)]
         #:unlink [u (list "unlink" #f)]
         #:rename [rn (list "rename" #f)]
         #:read-extattr [rea (list "read-extattr" #f)]
         #:write-extattr [wea (list "write-extattr" #f)])
  (dir-proxy (list p c crd crf al asl rsl uf us ud rnf rnt l lf rf wf af x s cl chr chd chf chm cho cht u rn rea wea) #f))


(define (file/c
         #:path [p (list "path" #f)]
         #:read [r (list "read" #f)]
         #:write [w (list "write" #f)]
         #:append [a (list "append" #f)]
         #:link [l (list "link" #f)]
         #:exec [x (list "exec" #f)]
         #:stat [s (list "stat" #f)]
         #:close [c (list "close" #f)]
         #:chflags [chf (list "chflags" #f)]
         #:chmod [chm (list "chmod" #f)]
         #:chown [cho (list "chown" #f)]
         #:chtimes [cht (list "chtimes" #f)]
         #:unlink [u (list "unlink" #f)]
         #:rename [rn (list "rename" #f)]
         #:read-extattr [rea (list "read-extattr" #f)]
         #:write-extattr [wea (list "write-extattr" #f)])
  (file-proxy (list p r w a l x s c chf chm cho cht u rn rea wea) #f))

(define (pipe-end/c 
         #:read [r (list "read" #f)]
         #:write [w (list "write" #f)]
         #:append [a (list "append" #f)]
         #:stat [s (list "stat" #f)]
         #:close [c (list "close" #f)])
  (define p (list "path" #f))
  (define l (list "link" #f))
  (define x (list "exec" #f))
  (pipe-end-proxy (list p r w a l x s c) #f))

(define (pipe-factory/c 
         #:first [fpe '()]
         #:second [spe '()])
  (pipe-factory-proxy (list fpe spe) #f))

(define (pipe-end? file) (and (shill-file? file) (symbol=? (shill-file-type file) 'pipe-end)))

(define (contents dir) ((shill-dir-contents dir)))

(define (create-dir dir path) ((shill-dir-create-dir dir) path))

(define (create-file dir path) ((shill-dir-create-file dir) path))

(define (add-link dir name file) ((shill-dir-add-link dir) name file))

(define (add-symlink dir name path) ((shill-dir-add-symlink dir) name path))

(define (read-symlink dir name) ((shill-dir-read-symlink dir) name))

(define (unlink-dir dir target name) ((shill-dir-unlink-dir dir) target name))
(define (unlink-file dir target name) ((shill-dir-unlink-file dir) target name))
(define (unlink-symlink dir name) ((shill-dir-unlink-symlink dir) name))

(define (rename source-dir target old-name target-dir new-name)
  ((shill-dir-rename source-dir) target old-name target-dir new-name))

(define (lookup dir name [mode #f]) ((shill-dir-lookup dir) name mode))

(define (chdir dir) ((shill-dir-chdir dir)))

(define (read file) ((shill-file-read file)))

(define (write file s) ((shill-file-write file) s))

(define (append file s) ((shill-file-append file) s))

(define (exec file #:stdin [stdin #f] #:stdout [stdout #f] #:stderr [stderr #f] #:extra [extra '()] #:timeout [time #f] args)
  (let ([rights (pick-node-rights file)])
    ((shill-file-exec file) rights args stdin stdout stderr extra time)))


(define (path node)
  (cond [(shill-dir? node) ((shill-dir-path node))]
        [(shill-file? node) ((shill-file-path node))]))

(define (size node)
  (cond [(shill-dir? node) (stat-size ((shill-dir-stat node)))]
	[(shill-file? node) (stat-size ((shill-file-stat node)))]))

(define (accessed node)
  (cond [(shill-dir? node) (stat-accessed ((shill-dir-stat node)))]
	[(shill-file? node) (stat-accessed ((shill-file-stat node)))]))

(define (modified node)
  (cond [(shill-dir? node) (stat-modified ((shill-dir-stat node)))]
	[(shill-file? node) (stat-modified ((shill-file-stat node)))]))

(define (status-changed node)
  (cond [(shill-dir? node) (stat-changed ((shill-dir-stat node)))]
	[(shill-file? node) (stat-changed ((shill-file-stat node)))]))

(define (created node)
  (cond [(shill-dir? node) (stat-created ((shill-dir-stat node)))]
	[(shill-file? node) (stat-created ((shill-file-stat node)))]))

(define (uid node)
  (cond [(shill-dir? node) (stat-uid ((shill-dir-stat node)))]
	[(shill-file? node) (stat-uid ((shill-file-stat node)))]))

(define (gid node)
  (cond [(shill-dir? node) (stat-gid ((shill-dir-stat node)))]
	[(shill-file? node) (stat-gid ((shill-file-stat node)))]))

(define (suid node)
  (cond [(shill-dir? node) (stat-suid ((shill-dir-stat node)))]
	[(shill-file? node) (stat-suid ((shill-file-stat node)))]))

(define (sgid node)
  (cond [(shill-dir? node) (stat-sgid ((shill-dir-stat node)))]
	[(shill-file? node) (stat-sgid ((shill-file-stat node)))]))

(define (perms node)
  (cond [(shill-dir? node) (stat-perms ((shill-dir-stat node)))]
	[(shill-file? node) (stat-perms ((shill-file-stat node)))]))

(define (close node)
  (cond [(shill-dir? node)
         ((shill-dir-close node))
         (set-desc-fd! (shill-dir-desc node) -1)]
        [(shill-file? node)
         ((shill-file-close node))
         (set-desc-fd! (shill-file-desc node) -1)]))

(define (open-dir target-path)
  (define p (string->path target-path))
  (let ([maybe-fd (rt:open-dir p)])
    (cond [(sys-error? maybe-fd) maybe-fd]
          [else (build-dir maybe-fd target-path)])))

(define (open-file target-path [mode #f])
  (define p (string->path target-path))
  (if mode 
      (let ([maybe-fd-exec (rt:open-for-exec p)])
        (cond [(sys-error? maybe-fd-exec) maybe-fd-exec]
              [else (build-file maybe-fd-exec target-path)]))
      (let ([maybe-fd (rt:open-file p)])
        (cond [(sys-error? maybe-fd) maybe-fd]
              [else (build-file maybe-fd target-path)]))))

(define (create-pipe pf)
  ((shill-pipe-factory-create pf)))

(define pipe-factory (shill-pipe-factory (thunk (map build-pipe-end (rt:create-pipe)))))

(define stdout (build-pipe-end (rt:stdout)))

(define stdin (build-pipe-end (rt:stdout)))

(define stderr (build-pipe-end (rt:stdout)))

(define cwd
  (build-dir (rt:cwd) "."))

(define (cap? cap) (or (shill-file? cap)
                       (pipe-end? cap)
                       (shill-dir? cap)
                       (shill-pipe-factory? cap)
                       (socket-factory? cap)))
