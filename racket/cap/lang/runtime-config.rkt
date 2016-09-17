#lang racket/base

(provide configure run-in-prompt)

(require racket/string)

(require shill/ambient/parse)
(require shill/private/repl)
(require (only-in shill/private/filesystem read))

(define old-eval (current-eval))
(define old-print (current-print))

(define (configure data)
  (define out-reader
    (thread
     (lambda ()
       (define (loop)
         (let ([newbytes (read replout-in)])
           (when (> (bytes-length newbytes) 0)
             (write-bytes newbytes))
           (loop)))
       (loop))))
  (define err-reader
    (thread
     (lambda ()
       (define (loop)
         (let ([newbytes (read replerr-in)])
           (when (> (bytes-length newbytes) 0)
             (write-bytes newbytes (current-error-port)))
           (loop)))
       (loop))))
  (let ([first? #t])
    (current-eval
     (lambda (form)
       (when first?
         (set! first? #f)
         (old-eval
          '(#%require (only shill/private/out
                            open-dir
                            open-file
                            cwd
                            stdin
                            pipe-factory
                            socket-factory
                            create-pipe
                            file?
                            dir?
                            pipe-factory?
                            socket-factory?
                            pipe-end?)))
         (old-eval
          '(#%require (only shill/private/repl
                            stdout
                            stderr)))
         (old-eval
          '(#%require (only shill/private/contract-utils
                            create-wallet
                            put
                            get)))
         (old-eval
          '(#%require (only shill/private/source-utils
                            var
                            set-var))))
       (old-eval form)))
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
           eof)))
    (current-print
     (lambda (val)
       (old-print val)))))

(define (run-in-prompt thunk)
  (define out-reader
    (thread
     (lambda ()
       (define (loop)
         (let ([newbytes (read replout-in)])
           (when (> (bytes-length newbytes) 0)
             (write-bytes newbytes))
           (loop)))
       (loop))))
  (define err-reader
    (thread
     (lambda ()
       (define (loop)
         (let ([newbytes (read replerr-in)])
           (when (> (bytes-length newbytes) 0)
             (write-bytes newbytes (current-error-port)))
           (loop)))
       (loop))))
  (let ([first? #t])
    (parameterize ([current-eval (lambda (form)
                                   (when first?
                                     (set! first? #f)
                                     (old-eval
                                      '(#%require (only shill/private/out
                                                        open-dir
                                                        open-file
                                                        cwd
                                                        stdin
                                                        pipe-factory
                                                        socket-factory
                                                        create-pipe
                                                        file?
                                                        dir?
                                                        pipe-factory?
                                                        socket-factory?
                                                        pipe-end?)))
                                     (old-eval
                                      '(#%require (only shill/private/repl
                                                        stdout
                                                        stderr)))
                                     (old-eval
                                      '(#%require (only shill/private/contract-utils
                                                        create-wallet
                                                        put
                                                        get)))
                                     (old-eval
                                      '(#%require (only shill/private/source-utils
                                                        var
                                                        set-var))))
                                   (old-eval form))]
                   [current-read-interaction (lambda (src port)
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
                                                   eof))]
                   [current-print (lambda (val)
                                    (thread
                                     (lambda ()
                                      (sleep 1)
                                      (kill-thread out-reader)
                                      (kill-thread err-reader)))
                                    (old-print val))])
          (thunk))))
