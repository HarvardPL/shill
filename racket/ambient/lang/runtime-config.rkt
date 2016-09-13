#lang racket/base

(provide configure)

(require shill/ambient/parse)
(require shill/private/repl)
(require (only-in shill/private/filesystem read))

(define out-reader
  (thread
   (lambda ()
     (let ([buffer (make-bytes 0)])
       (define (loop)
         (sleep 0.05)
         (set! buffer (bytes-append buffer (read replout-in)))
         (let ([msg (thread-try-receive)])
           (when (thread? msg)
             (thread-send msg buffer)
             (set! buffer (make-bytes 0))))
         (loop))
       (loop)))))

(define err-reader
  (thread
   (lambda ()
     (let ([buffer (make-bytes 0)])
       (define (loop)
         (sleep 0.05)
         (set! buffer (bytes-append buffer (read replerr-in)))
         (let ([msg (thread-try-receive)])
           (when (thread? msg)
             (thread-send msg buffer)
             (set! buffer (make-bytes 0)))
           (loop)))
       (loop)))))

(define (get-out)
  (thread-send out-reader (current-thread))
  (thread-receive))
(define (get-err)
  (thread-send err-reader (current-thread))
  (thread-receive))

(define (configure data)
  (let ([old-eval (current-eval)]
        [old-print (current-print)]
        [first? #t])
    (current-eval
     (lambda (form)
       (when first?
         (old-eval
          '(#%require (only shill/private/repl
                            stdout
                            stderr)))
         (set! first? #f))
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
       (old-print val)
       (let ([out (get-out)]
             [err (get-err)])
         (unless (eq? (bytes-length out) 0)
           (printf "stdout:~n~a~n" (bytes->string/utf-8 out)))
         (unless (eq? (bytes-length err) 0)
           (printf "stderr:~n~a~n" (bytes->string/utf-8 err))))))))