#lang racket

(provide (struct-out desc)
         (struct-out dir)
	 (struct-out stat)
         _node
         node?
         node-fd
         sys-error?
         open-file
         open-for-exec
         open-dir
         close-node
         contents
         lookup-file
         lookup-for-exec
         lookup-dir
         read-link
         add-link
         add-symlink
         unlink-file
         unlink-symlink
         unlink-dir
         rename-node
         add-file
         create-file
         create-dir
         read-file
         append-file
         write-file
         new-pipe
         read-pipe
         write-pipe
         cwd
         chdir
         stdin
         stdout
         stderr
	 shill-stat
	 (contract-out   [path (-> node? (or/c path? #f))])
         #;(contract-out [open-file (-> path? (maybe/c desc?))]
                         [open-for-exec (-> path? (maybe/c desc?))]
                         [open-dir (-> path? (maybe/c dir?))]
                         [close-node (-> node? (maybe/c void?))]
                         [contents (-> dir? (maybe/c (listof path-string?)))]
                         [lookup-file (-> dir? path-element? (maybe/c desc?))]
                         [lookup-for-exec (-> dir? path-element? (maybe/c desc?))]
                         [lookup-dir (-> dir? path-element? (maybe/c dir?))]
                         [read-link (-> dir? path-element? (maybe/c path?))]
                         [add-link (-> dir? path-element? desc? (maybe/c void?))]
                         [add-symlink (-> dir? path-element? path? (maybe/c void?))]
                         [unlink-file (-> dir? path-element? node? (maybe/c void?))]
                         [unlink-symlink (-> dir? path-element? (maybe/c void?))]
                         [unlink-dir (-> dir? path-element? node? (maybe/c void?))]
                         [rename-node (-> dir? path-element? dir? path-element? (maybe/c void?))]
                         [add-file (-> dir? path-element? desc? (maybe/c void?))]
                         [create-file (-> dir? path-element? (maybe/c desc?))]
                         [create-dir (-> dir? path-element? (maybe/c dir?))]
                         [read-file (-> desc? (maybe/c bytes?))]
                         [append-file (-> desc? bytes? (maybe/c void?))]
                         [write-file (-> desc? bytes? (maybe/c void?))]
                         [new-pipe (-> (maybe/c (list/c desc? desc?)))]
                         [read-pipe (-> desc? (maybe/c bytes?))]
                         [write-pipe (-> desc? bytes? (maybe/c void?))]
                         [cwd (-> (maybe/c dir?))]
                         [chdir (-> dir? (maybe/c void?))]
                         [stdin (-> (maybe/c desc?))]
                         [stdout (-> (maybe/c desc?))]
                         [stderr (-> (maybe/c desc?))]))

#|
FFI implementations
|#
  
  (require (rename-in ffi/unsafe [-> ffi:->]))
  (require ffi/cvector)
  (require ffi/unsafe/alloc)
  
  ; Compute C ffi interfaces on demand
  (define interfaces (make-hash))
  
  ; Bind a C object (cname : string?) with (type : ctype?) to a
  ; racket binding (name : string?).
  ; Raises an exception if the binding could not be made
  (define (bind-interface name #:cname [cname #f] lib type)
    (hash-ref interfaces name
              (λ ()
                (let ([i (get-ffi-obj (if cname cname name) lib type #f)])
                  (hash-set! interfaces name i)
                  i))))
  
  (define (bind-stdlib name #:cname [cname #f] type)
    (bind-interface name #:cname cname #f type))
  
  (define shillrt
    (ffi-lib "libshillrt"))
  
  (define (bind-shillrt name type)
    (bind-interface name shillrt type))
  
  ; Retrieve the error message corresponding to an errno value
  (define (strerror errno)
    (let ([c-strerror (bind-stdlib "strerror" (_fun _int ffi:-> _string))])
      (c-strerror errno)))
  
  ; Retrieve the error message for the most recently saved errno
  (define (saved-error)
    (strerror (saved-errno)))
  
  
   ;; printer extention for sys-error
  (define (sys-error-print sys-error port mode)
    (define msg (string-append "system call error: " (sys-error-msg sys-error)))
    (write-string msg port))
  
  ; system call error carrier structure
  (struct sys-error (msg)
    #:methods gen:custom-write
    [(define write-proc sys-error-print)])
  
  ;; sys-error maybe-like contract
  (define (maybe/c ctc) (or/c ctc sys-error?))
  
  (struct desc ([fd #:mutable]) #:transparent)
  (struct dir ([fd #:mutable]) #:transparent)
  
  (define (node? node)
    (or (desc? node) (dir? node)))
  
  (define (node-fd node)
    (cond
      [(desc? node) (desc-fd node)]
      [(dir? node) (dir-fd node)]))
  
  (define _desc (make-ctype _int
                            (λ (desc) (desc-fd desc))
                            (λ (n) (desc n))))
  (define _dir (make-ctype _int
                           (λ (dir) (dir-fd dir))
                           (λ (n) (dir n))))

  (define _node (make-ctype _int
                            (λ (node) (if node (node-fd node) -1))
                            (λ (i) (error "_cnode is one-way"))))
  
  (define (node-dealloc n)
    (match n
      [(desc f) (close-node n)]
      [(dir f) (close-node n)]
      [(sys-error _) #t]))

  (define (open-file-internal path)
    (let* ([shill-openfile (bind-shillrt "shill_openfile" (_fun #:save-errno 'posix _path ffi:-> _desc))]
           [file (shill-openfile path)])
      (if (eq? (desc-fd file) -1)
          (sys-error (saved-error))
          file)))
  
  (define open-file
    ((allocator node-dealloc) open-file-internal))
  
  (define (open-for-exec-internal path)
    (let* ([shill-openforexec (bind-shillrt "shill_openforexec" (_fun #:save-errno 'posix _path ffi:-> _desc))]
           [file (shill-openforexec path)])
      (if (eq? (desc-fd file) -1)
          (sys-error (saved-error))
          file)))
  
  (define open-for-exec
    ((allocator node-dealloc) open-for-exec-internal))
  
  (define (open-dir-internal path)
    (let* ([shill-opendir (bind-shillrt "shill_opendir" (_fun #:save-errno 'posix _path ffi:-> _dir))]
           [dir (shill-opendir path)])
      (if (eq? (dir-fd dir) -1)
          (sys-error (saved-error))
          dir)))
  
  (define open-dir
    ((allocator node-dealloc) open-dir-internal))
  
  (define (cwd-internal)
    (let* ([shill-cwd (bind-shillrt "shill_cwd" (_fun #:save-errno 'posix ffi:-> _dir))]
	   [dir (shill-cwd)])
      (if (eq? (dir-fd dir) -1)
	  (sys-error (saved-error))
	  dir)))
  
  (define cwd
    ((allocator node-dealloc) cwd-internal))
  
  (define (stdin-internal)
    (let* ([shill-stdin (bind-shillrt "shill_stdin" (_fun #:save-errno 'posix ffi:-> _desc))]
	   [in (shill-stdin)])
      (if (eq? (desc-fd in) -1)
	  (sys-error (saved-error))
	  in)))
  
  (define stdin
    ((allocator node-dealloc) stdin-internal))

  (define (chdir dir)
    (let* ([shill-chdir (bind-shillrt "shill_chdir" (_fun #:save-errno 'posix _dir ffi:-> _int))]
           [err (shill-chdir dir)])
      (when (eq? err -1)
          (sys-error (saved-error)))))
  
  (define (stdout-internal)
    (let* ([shill-stdout (bind-shillrt "shill_stdout" (_fun #:save-errno 'posix ffi:-> _desc))]
	   [out (shill-stdout)])
      (if (eq? (desc-fd out) -1)
	  (sys-error (saved-error))
	  out)))
  
  (define stdout
    ((allocator node-dealloc) stdout-internal))
  
  (define (stderr-internal)
    (let* ([shill-stderr (bind-shillrt "shill_stderr" (_fun #:save-errno 'posix ffi:-> _desc))]
	   [err (shill-stderr)])
      (if (eq? (desc-fd err) -1)
	  (sys-error (saved-error))
	  err)))
  
  (define stderr
    ((allocator node-dealloc) stderr-internal))

  (define (new-pipe)
    (let* ([pipetype (_fun #:save-errno 'posix _cvector ffi:-> _int)]
	   [pipe (bind-shillrt "shill_pipe" pipetype)]
	   [filedes (cvector _int -1 -1)]
           [wrapper ((allocator node-dealloc) (lambda (fd) (desc fd)))]
	   [ret (pipe filedes)])
      (if (eq? ret -1)
	  (sys-error (saved-error))
	  (list (wrapper (cvector-ref filedes 0))
                (wrapper (cvector-ref filedes 1))))))
    
  (define (close-node node)
    (let* ([c-close (bind-stdlib "close" (_fun #:save-errno 'posix _desc ffi:-> _int))]
           [c-closedir (bind-stdlib "closedir" #:cname "close" (_fun #:save-errno 'posix _dir ffi:-> _int))]
           [ret (cond
                  [(desc? node) (c-close node)]
                  [(dir? node) (c-closedir node)]
                  [else (raise-argument-error 'close-node "node?" node)])])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (lookup-dir-internal dir name)
    (let* ([shill-lookup-dir (bind-shillrt "shill_lookupdir" (_fun #:save-errno 'posix _dir _path ffi:-> _dir))]
           [dir (shill-lookup-dir dir name)])
      (if (eq? (dir-fd dir) -1) (sys-error (saved-error)) dir)))
  
  (define lookup-dir
    ((allocator node-dealloc) lookup-dir-internal))
  
  (define (lookup-file-internal dir name)
    (let* ([shill-lookup-file (bind-shillrt "shill_lookupfile" (_fun #:save-errno 'posix _dir _path ffi:-> _desc))]
           [file (shill-lookup-file dir name)])
      (if (eq? (desc-fd file) -1) (sys-error (saved-error)) file)))
  
  (define lookup-file
    ((allocator node-dealloc) lookup-file-internal))
  
  (define (lookup-for-exec-internal dir name)
    (let* ([shill-lookup-for-exec (bind-shillrt "shill_lookupforexec" (_fun #:save-errno 'posix _dir _path ffi:-> _desc))]
           [file (shill-lookup-for-exec dir name)])
      (if (eq? (desc-fd file) -1) (sys-error (saved-error)) file)))
  
  (define lookup-for-exec
    ((allocator node-dealloc) lookup-for-exec-internal))
  
  (define (read-link dir name)
    (let* ([shill-readlink (bind-shillrt "shill_readlink" (_fun #:save-errno 'posix _dir _path (_fun #:keep #f _path ffi:-> _void) ffi:-> _int))]
           [link #f]
           [ret (shill-readlink dir name (λ (p) (set! link p)))])
      (if (eq? ret -1)
        (sys-error (saved-error))
        link)))
  
  (define (add-link dir name file)
    (let* ([shill-addlink (bind-shillrt "shill_addlink" (_fun #:save-errno 'posix _dir _path _desc ffi:-> _int))]
           [ret (shill-addlink dir name file)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (add-symlink dir name path)
    (let* ([shill-addsymlink (bind-shillrt "shill_addsymlink" (_fun #:save-errno 'posix _dir _path _path ffi:-> _int))]
           [ret (shill-addsymlink dir name path)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (unlink-file dir name tgt)
    (let* ([shill-unlink (bind-shillrt "shill_unlinkfile"
                                       (_fun #:save-errno 'posix _dir _path _node ffi:-> _int))]
           [ret (shill-unlink dir name tgt)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))

  (define (unlink-symlink dir name)
    (let* ([shill-unlink (bind-shillrt "shill_rmsymlink"
                                       (_fun #:save-errno 'posix _dir _path ffi:-> _int))]
           [ret (shill-unlink dir name)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (unlink-dir dir name tgt)
    (let* ([shill-unlink (bind-shillrt "shill_unlinkdir"
                                       (_fun #:save-errno 'posix _dir _path _node ffi:-> _int))]
           [ret (shill-unlink dir name tgt)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (rename-node olddir old newdir new)
    (let* ([shill-rename (bind-shillrt "shill_rename" (_fun #:save-errno 'posix _dir _path _dir _path ffi:-> _int))]
           [ret (shill-rename olddir old newdir new)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (add-file-internal dir name file)
    (let* ([shill-add-file (bind-shillrt "shill_addfile" (_fun #:save-errno 'posix _dir _desc _path ffi:-> _desc))]
           [ret (shill-add-file dir file name)])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define add-file
    ((allocator node-dealloc) add-file-internal))
  
  (define (create-file-internal dir name)
    (let* ([shill-create-file (bind-shillrt "shill_createfile" (_fun #:save-errno 'posix _dir _path ffi:-> _desc))]
           [file (shill-create-file dir name)])
      (if (eq? (desc-fd file) -1)
          (sys-error (saved-error))
          file)))
  
  (define create-file
    ((allocator node-dealloc) create-file-internal))
  
  (define (create-dir-internal dir name)
    (let* ([shill-create-dir (bind-shillrt "shill_createdir" (_fun #:save-errno 'posix _dir _path ffi:-> _dir))]
           [dir (shill-create-dir dir name)])
      (if (eq? (dir-fd dir) -1) (sys-error (saved-error)) dir)))
  
  (define create-dir
    ((allocator node-dealloc) create-dir-internal))
  
  (define (file-length file)
    (let* ([shill-file-length (bind-shillrt "shill_filelength" (_fun #:save-errno 'posix _desc ffi:-> _int))]
           [ret (shill-file-length file)])
      (if (eq? ret -1)
          (sys-error (saved-error))
          ret)))
  
  (define (file-read file buf)
    (let* ([shill-file-read (bind-shillrt "shill_read" (_fun #:save-errno 'posix _desc _gcpointer _int ffi:-> _int))]
           [ret (shill-file-read file buf (bytes-length buf))])
      (if (eq? ret -1)
          (sys-error (saved-error))
          buf)))

  (define (read-file file)
    (let ([len (file-length file)])
      (if (sys-error? len)
	  len
	  (file-read file (make-bytes len 0)))))

  (define (read-pipe-once pipe)
    (let* ([readpipe-type (_fun #:save-errno 'posix _desc _gcpointer _int ffi:-> _int)]
	   [shill-read-pipe (bind-shillrt "shill_readpipe" readpipe-type)]
	   [buf (make-bytes 1024 0)]
	   [ret (shill-read-pipe pipe buf 1024)])
      (match ret
	[-1 (sys-error (saved-error))]
	[0 (make-bytes 0)]
	[_ (let ([readbytes (make-bytes ret)])
	     (bytes-copy! readbytes 0 buf 0 ret)
	     readbytes)])))

  (define (read-pipe pipe)
    (let* ([bytes (read-pipe-once pipe)])
      (if (sys-error? bytes)
          bytes
          (if (eq? 1024 (bytes-length bytes))
              (bytes-append bytes (read-pipe pipe))
	      bytes))))
    
  (define (write-file file bytes)
    (let* ([shill-file-write (bind-shillrt "shill_write" (_fun #:save-errno 'posix _desc _gcpointer _int ffi:-> _int))]
           [ret (shill-file-write file bytes (bytes-length bytes))])
      (when (eq? ret -1)
        (sys-error (saved-error)))))
  
  (define (append-file file bytes)
    (let* ([shill-file-append (bind-shillrt "shill_append" (_fun #:save-errno 'posix _desc _gcpointer _int ffi:-> _int))]
           [ret (shill-file-append file bytes (bytes-length bytes))])
      (when (eq? ret -1)
        (sys-error (saved-error)))))

  (define (write-pipe pipe bytes)
    (let* ([pipe-write-type (_fun #:save-errno 'posix _desc _gcpointer _int ffi:-> _int)]
	   [shill-pipe-write (bind-shillrt "shill_writepipe" pipe-write-type)]
	   [ret (shill-pipe-write pipe bytes (bytes-length bytes))])
      (when (eq? ret -1)
	(sys-error (saved-error)))))
  
  (define (contents dir)
    (let* ([shill-contents (bind-shillrt "shill_contents"
                                         (_fun #:save-errno 'posix _dir (_fun #:keep #f _path ffi:-> _void) ffi:-> _int))]
           [contents '()]
           [ret (shill-contents dir (λ (p) 
                                       (define-values (base name dir) (split-path p))
                                       (unless (or (equal? name 'up)
                                                   (equal? name 'same))
                                         (set! contents (cons (path->string p) contents)))))])
      (if (eq? ret -1)
        (sys-error (saved-error))
        (reverse contents))))
  
  (define (path node)
    (let* ([shill-path (bind-shillrt "shill_path"
                                     (_fun #:save-errno 'posix _int (_fun #:keep #f _string ffi:-> _void) ffi:-> _int))]
           [path #f]
           [ret (shill-path (node-fd node) (λ (p) (set! path (string->path p))))])
      path))

  (define-struct stat (size accessed modified changed created))

  (define-cstruct _cstat ([size  _uint64]
			  [asec  _uint64]
			  [ansec _uint64]
			  [msec  _uint64]
			  [mnsec _uint64]
			  [csec  _uint64]
			  [cnsec _uint64]
			  [bsec  _uint64]
			  [bnsec _uint64]))

  (define (cstat-to-stat cs)
    (stat (cstat-size cs)
	  (times-to-nanoseconds (cstat-asec cs) (cstat-ansec cs))
	  (times-to-nanoseconds (cstat-msec cs) (cstat-mnsec cs))
	  (times-to-nanoseconds (cstat-csec cs) (cstat-cnsec cs))
	  (times-to-nanoseconds (cstat-bsec cs) (cstat-bnsec cs))))

  (define (times-to-nanoseconds sec nano)
    (+ (* 1000000000 sec) nano))

  (define (shill-stat node)
    (let* ([stat (bind-shillrt "shill_stat"
			       (_fun #:save-errno 'posix _int _cstat-pointer ffi:-> _int))]
	   [cstat-buf (make-cstat 0 0 0 0 0 0 0 0 0)]
	   [ret (stat (node-fd node) cstat-buf)])
    (if (eq? ret -1)
	(sys-error (saved-error))
	(cstat-to-stat cstat-buf))))
