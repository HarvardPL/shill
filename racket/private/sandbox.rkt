#lang racket

(provide shill-sandbox 
   #;(contract-out [shill-sandbox (-> desc?
                                          (or/c desc? #f)
                                          (or/c desc? #f)
                                          (or/c desc? #f)
                                          (listof node?)
                                          (listof (listof right?))
                                          (listof (list/c (listof right-flag?) (listof right-flag?)))
                                          list? ; this should be more precise for socket caps
                                          (listof string?)
                                          (or/c #f (and/c positive? fixnum?))
                                          integer?)]))


(require (rename-in ffi/unsafe [-> ffi:->])
         "ffi.rkt")

(module+ test
  (require rackunit))

(define (strerror errno)
  (let ([c-strerror (get-ffi-obj "strerror" #f (_fun _int ffi:-> _string))])
    (c-strerror errno)))

(define (saved-error)
  (strerror (saved-errno)))

(define-cstruct _shill_cap ([flags _int]
                            [lookup _shill_cap-pointer/null]
                            [create-file _shill_cap-pointer/null]
                            [create-dir _shill_cap-pointer/null]))

(define (right-flag? r)
  (member r (list 'chdir 'chroot 'exec 'contents 'close
                  'add-link 'add-symlink 'unlink-file 'unlink-dir 'read-symlink
                  'read 'write 'append 'rename 'link 'unlink 'unlink-symlink
                  'chmod 'chown 'chflags 'read-extattr 'write-extattr
                  'chtimes 'stat 'path)))

(define (right-special? r)
  (and (list? r)
       (member (first r) (list 'lookup 'create-file 'create-dir))
       (or (false? (second r))
           (andmap right? (second r)))))

(define (right-for-socket? r)
  (member r '(send recv accept bind connect listen poll visible stat)))

(define (right? r)
  (or (right-flag? r)
      (right-special? r)
      (right-for-socket? r)))

(define/match (right->shill_cap_flag right)
  [('close) #x0]
  [('chdir) #x1]
  [('chroot) #x2]
  [((list 'create-file _)) #x4]
  [((list 'create-dir _)) #x8]
  [('exec) #x10]
  [((cons 'lookup _)) #x20]
  [('contents) #x40]
  [('add-link) #x100]
  [('link) #x200]
  [('unlink-file) #x400]
  [('unlink-dir) #x800]
  [('unlink) #x1000000]
  [('read) #x1000]
  [('write) #x2000]
  [('append) #x4000]
  [('chmod) #x10000]
  [('chown) #x20000]
  [('chflags) #x40000]
  [('chtimes) #x80000]
  [('stat) #x100000]
  [('read-symlink) #x200000]
  [('add-symlink) #x400000]
  [('path) #x0]
  [('rename) #x0]
  [('read-extattr) #x2000000]
  [('write-extattr) #x4000000]
  [('unlink-symlink) #x8000000])

;; Permission = [U 'read 'write 'stat 'connect 'bind 'listen 'accept]

;; StrippedSocketCap = [List Number
;;                           Number
;;                           [Listof [List Number Number]]]
;;                           [Listof Permission]]

;; socket-factory-caps->c-netcaps : [Listof StrippedSocketCap]
;;                                  ->
;;                                  [Listof Number]
;;
;; Encodes the socket factory caps into the arguments which will be given the
;; mac system call. The C world expects a uint64_t array of size 3:
;;
;;     uint64_t args[3] = { ADDRESS_FAMILY, SOCKET_TYPE, PERMISSION_BIT_VECTOR }
;;
;; the address family and socket type are already represented as numbers. The
;; permissions are converted according to the C macros which specify the values
;; of the socket permissions.
;;
;; We choose to send a flattened array for simplicity of the interface with C
;; (an array of three element structures wouldn't look any different in C
;; anyway).
(define (socket-factory-caps->c-netcaps socket-factory-caps)
  (for/fold
      ([c-args '()])
      ([socket-factory-cap socket-factory-caps])
    (append (socket-factory-cap->c-netcap socket-factory-cap)
            c-args)))

;; socket-factory-cap->c-netcap : StrippedSocketCap
;;                                ->
;;                                [List Number Number Number]
;;
;; Converts the socket factory cap into the three arguments that the C world
;; expects, namely the address family, the socket type, and the permission
;; set. All of these values are represented as unsigned 64 bit integers.
(define/match (socket-factory-cap->c-netcap cap)
  [((list address-family socket-type _ permissions))
   (list address-family socket-type (permissions->bit-vector permissions))])
(module+ test
  (check-equal? (socket-factory-cap->c-netcap '(1 1 () (send recv connect)))
                (list 1 1 19)))

;; permissions->bit-vector : [Listof Permission] -> Number
(define (permissions->bit-vector permissions)
  (for/sum ([permission permissions])
    (permission->value permission)))
(module+ test
  (check-equal? (permissions->bit-vector '()) 0)
  (check-equal? (permissions->bit-vector '(send recv bind connect listen))
                (+ 1 2 8 16 32)))

;; permission->value : Permission -> Number
;;
;; The values for this match are pulled from kmod/shill-socket-permissions.h, in
;; fact we should probably autogenerate both that file and this function from a
;; file with a sexp.
(define (permission->value permission)
  (match permission
    ['send    #x00000001]
    ['recv    #x00000002]
    ['write   #x00000001]
    ['read    #x00000002]
    ['accept  #x00000004]
    ['bind    #x00000008]
    ['connect #x00000010]
    ['listen  #x00000020]
    ['poll    #x00000040]
    ['stat    #x00000080]
    ['visible #x00000100]
    ))

(define (rights->shill_cap rights)
  (and rights
       (let ([flags (foldl (λ (r f) (bitwise-ior (right->shill_cap_flag r) f)) 0 rights)]
             [lookup (findf (λ (r) (match r [(list 'lookup _) #t] [_ #f])) rights)]
             [create-file (findf (λ (r) (match r [(list 'create-file _) #t] [_ #f])) rights)]
             [create-dir (findf (λ (r) (match r [(list 'create-dir _) #t] [_ #f])) rights)])
         (make-shill_cap flags
                         (and lookup (rights->shill_cap (second lookup)))
                         (and create-file (rights->shill_cap (second create-file)))
                         (and create-dir (rights->shill_cap (second create-dir)))))))

(define (merge-pipe-factories rights)
  (define (merge s1 s2)
    (cond
     [(subset? s1 s2) s2]
     [(subset? s2 s1) s1]
     [else (error "incompatible pipe factories")]))
  (define (flatten pf-rights)
    (list->set (append (first pf-rights) (second pf-rights))))
  (let ([merged (foldl merge (set) (map flatten rights))])
    (if (empty? merged) #f (rights->shill_cap (set->list merged)))))

(define (shill-sandbox prog stdin stdout stderr fds node-caps pipe-factory-caps socket-factory-caps args timeout)
  (let* ([rights (map rights->shill_cap node-caps)]
         [c-netcaps (socket-factory-caps->c-netcaps socket-factory-caps)]
         [_fd_array (_array/list _node (length fds))]
         [_shill_cap-pointer_array (_array/list _shill_cap-pointer/null (length rights))]
         [_string-pointer_array (_array/list _string (+ (length args) 1))]
         [c-sandbox-type (_fun #:save-errno 'posix
                               _node _node _node _node _fd_array _shill_cap-pointer_array
                               _int _int64 _bool _string-pointer_array
                               _gcpointer _uint64 _shill_cap-pointer/null
                               ffi:-> _int)]
         [c-sandbox (get-ffi-obj "shill_sandbox" (ffi-lib "libshillrt") c-sandbox-type)]
         [lim (min (length fds) (length rights))]
         [timelim (if timeout timeout -1)]
         [pipefactory (merge-pipe-factories pipe-factory-caps)]
         [ret (c-sandbox prog
                         stdin stdout stderr
                         fds rights lim timelim #f
                         (append args (list #f))
                         (list->cblock c-netcaps _uint64)
                         (length c-netcaps)
                         pipefactory)])
    (when (eq? ret -1)
      (error (saved-error)))
    ret))
