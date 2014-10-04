#lang racket

(require rackunit)
(require shill/private/ffi
         shill/private/sandbox
         (only-in shill/private/filesystem
                  create-pipe
                  pipe-factory))

;; cap-pairs->lists : [Listof [List Object Capability]]
;;                    ->
;;                    [Listof Object]
;;                    [Listof Capability]
(define (cap-pairs->lists pairs)
  (let ((things (map first pairs))
        (caps (map second pairs)))
    (values things caps)))

(test-case
 "Run telnet"
 ;; hack
 ;; I cannot get pipes to work
 (define tmp (make-temporary-file "rkttmp~a" 'directory #f))
 (define root (open-dir tmp))
 (define out-name (string->path "out.txt"))
 (create-file root out-name)
 (define stdout (lookup-file root out-name))
 (define err-name (string->path "err.txt"))
 (create-file root err-name)
 (define stderr (lookup-file root err-name))
 (define in-name (string->path "in.txt"))
 (create-file root in-name)
 (define stdin (lookup-file root in-name))
 ;; the hack should be replaced with:
 ;; (match-define (list stdout-in stdout-out) (create-pipe pipe-factory))
 ;; endhack
 ;; (define curl (open-for-exec (string->path "/usr/local/bin/curl")))
 (define telnet (open-for-exec (string->path "/usr/bin/telnet")))
 (define-values (files caps)
   (cap-pairs->lists
    (list (list (open-dir (string->path "/"))
                '((lookup #f) stat))
          (list (open-dir (string->path "/dev/"))
                '((lookup (write)) stat))
          (list (open-file (string->path "/etc/libmap.conf"))
                '(read))
          (list (open-file (string->path "/libexec/ld-elf.so.1"))
                '(read exec))
          (list (open-file (string->path "/lib/libncurses.so.8"))
                '(read exec))
          (list (open-file (string->path "/lib/libipsec.so.4"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libmp.so.7"))
                '(read exec))
          (list (open-file (string->path "/lib/libcrypto.so.6"))
                '(read exec))
          (list (open-file (string->path "/lib/libcrypt.so.5"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libpam.so.5"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libkrb5.so.10"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libhx509.so.10"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libasn1.so.10"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libcom_err.so.5"))
                '(read exec))
          (list (open-file (string->path "/usr/lib/libroken.so.10"))
                '(read exec))
          (list (open-file (string->path "/lib/libc.so.7"))
                '(read exec))
          ;; (list (open-file (string->path "/usr/lib/libgssapi.so.10"))
          ;;       '(read exec))
          ;; (list (open-file (string->path "/lib/libz.so.6"))
          ;;       '(read exec))
          ;; (list (open-file (string->path "/lib/libthr.so.3"))
          ;;       '(read exec))
          (list (open-file (string->path "/usr/share/locale/en_US.UTF-8"))
                '(read))
          (list (open-file (string->path "/usr/share/locale/UTF-8/LC_CTYPE"))
                '(read))
          (list (open-file (string->path "/var/run/ld-elf.so.hints"))
                '(read))
          (list (open-file (string->path "/etc/nsswitch.conf"))
                '(read))
          (list telnet
                '(read exec))
          (list stdin
                '(read stat))
          (list stdout
                '(write append stat))
          (list stderr
                '(write append stat)))))
 (define socket-caps
   '((2 1 () (send recv connect poll))))

 (test-case
  "with appropraiate caps"
  (check-equal? (shill-sandbox telnet
                               stdin
                               stdout
                               stderr
                               files
                               caps
                               empty    ; pipe caps
                               socket-caps
                               (list "/usr/bin/telnet" "127.0.0.1" "22")
                               #f)
                1))
(test-case "expected stdout"
 (define expected-telnet-stdout
   "Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
")

 (check-equal? (bytes->string/utf-8 (read-file stdout))
               expected-telnet-stdout))
(test-case "expected stderr"
 (define expected-telnet-stderr
   "Connection closed by foreign host.
")

 (check-equal? (bytes->string/utf-8 (read-file stderr))
               expected-telnet-stderr))

 (close-node telnet)
 (delete-directory/files tmp))
