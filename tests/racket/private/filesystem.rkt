#lang racket


(require rackunit rackunit/text-ui shill/private/filesystem "../test-utils.rkt")


(define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))

(test-case
 "Create, read, and write a file"
 (define msg "Hello, world!")
 (define root (open-dir tmp))
 (create-file root "tmp.txt")
 (define txt (lookup root "tmp.txt"))
 (write txt (string->bytes/utf-8 msg))
 (define txt2 (lookup root "tmp.txt"))
 (check-equal? (bytes->string/utf-8 (read txt2)) msg)
 (unlink-file root txt "tmp.txt")
 (check-true  (not (file? (lookup root "tmp.txt")))))


(test-case
 "List the contents of a directory"
 (define root (open-dir tmp))
 (create-file root "file1.txt")
 (create-file root "file2.txt")
 (define content (contents root))
 (define files '("file1.txt" "file2.txt"))
 (check-equal? content files))

(test-case
 "Call path on a directory"
 (define root (open-dir tmp))
 (check-equal? tmp (path root)))

(test-case
 "Call path on a file"
 (define root (open-dir tmp))
 (define test (create-file root "pathtest.txt"))
 (check-equal? (path->string (path->complete-path (string->path "pathtest.txt") tmp)) (path test)))

(test-case
 "Create and read a symlink"
 (define root (open-dir tmp))
 (add-symlink root "link" "foobar")
 (check-equal? (read-symlink root  "link") "foobar"))

(test-case
 "Create and use a link"
 (define msg "Hello, world!")
 (define root (open-dir tmp))
 (create-file root "tmp2.txt")
 (define txt (lookup root "tmp2.txt"))
 (write txt (string->bytes/utf-8 msg))
 (add-link root "link.txt" txt)
 (unlink-file root txt "tmp2.txt")
 (define link (lookup root "link.txt"))
 (check-equal? (bytes->string/utf-8 (read link)) msg))

(test-case
 "Create, successful lookup, path, rename, failed lookup, succesful lookup, read"
 (define msg "Hello, world!")
 (define root (open-dir tmp))
 (create-file root "tmp2.txt")
 (define txt (lookup root "tmp2.txt"))
 (check-equal? (path txt) (string-append tmp "/tmp2.txt"))
 (append txt (string->bytes/utf-8 msg))
 (rename root txt "tmp2.txt" root "tmp3.txt")
 (check-equal? (file? (lookup root "tmp2.txt")) #f)
 (define txt-alt (lookup root "tmp3.txt"))
 (check-equal? (bytes->string/utf-8 (read txt-alt)) msg)
 (check-equal? 13 (size txt-alt)))

(test-pass
 "Run ls"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [ls-test (-> (file/c #:exec (list "exec" #t))
                                        (listof  (or/c (file/c #:read (list "read" #t) #:exec (list "exec" #t) #:stat (list "stat" #t))
                                                       (dir/c #:lookup (list "lookup" #t) 
                                                              #:read (list "read" #t))))
                                        (dir/c #:create-file (list "create-file" #t (list (list "read" #t) (list "write" #t) (list "append" #t)))
                                               #:lookup (list "lookup" #t) 
                                               #:read (list "read" #t) 
                                               #:write (list "write" #t)
                                               #:append (list "append" #t)
                                               #:path (list "path" #t)
                                               #:stat (list "stat" #t))
                                        any)]))
    (define (ls-test ls ls-files root)
      (define in-name "in.txt")
      (define out-name "out.txt")
      (create-file root in-name)
      (create-file root out-name)
      (define in (lookup root in-name))
      (define out (lookup root out-name))
      (exec ls #:stdin in #:stdout out #:stderr out #:extra ls-files (list "/bin/ls" root))
      (displayln "Finished sandbox!")
      (displayln (read out))))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define ls (open-file "/bin/ls"))
    (define root (open-dir tmp))
    (define ls-files
      (let* ([fsroot (open-dir "/")]
             [libmap (open-file "/etc/libmap.conf")]
             [ld-elf (open-file "/libexec/ld-elf.so.1")]
             [libc (open-file "/lib/libc.so.7")]
             [libutil (open-file "/lib/libutil.so.9")]
             [libncurses (open-file "/lib/libncurses.so.8")]
             [locale1 (open-file "/usr/share/locale/en_US.UTF-8")]
             [locale2 (open-file "/usr/share/locale/UTF-8/LC_CTYPE")]
             [ld-elf-hints (open-file "/var/run/ld-elf.so.hints")])
        (list ls fsroot libmap ld-elf libc libutil libncurses locale1 locale2 ld-elf-hints)))
    (ls-test ls ls-files root))
  (require 'ambient)))

(test-contract-fail
 "Run rm"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rm-test (-> (dir/c #:contents (list "contents" #t)
                                               #:lookup (list "lookup" #t)
                                               #:read-symlink (list "read-symlink" #t)
                                               #:read (list "read" #t)
                                               #:exec (list "exec" #t)
                                               #:stat (list "stat" #t))
                                        (file/c #:exec (list "exec" #t))
                                        (dir/c ;#:lookup (list "lookup" #t)
					       #:unlink-file (list "unlink-file" #t))
                                        path-string?
                                        any)]
                           [rm-nonsecret (-> (dir/c
					       #:lookup (list "lookup" #t)
					       #:unlink-file (list "unlink-file" #t (λ (s) (not (string=? s "secret.txt")))))
                                             (file/c #:rename (list "rename" #t))
                                             string?
                                             any)]))
    (define (rm-test root rm rm-in path)
      (exec rm #:extra (list root rm rm-in) (list "/bin/rm" path))
      (displayln "Finished sandbox!"))
    (define (rm-nonsecret dir target name)
      (unlink-file dir target name)))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define file1 (open-output-file (string-append tmp "/rm-file1.txt")))
    (define file2 (open-output-file (string-append tmp "/rm-file2.txt")))
    (define file3 (open-output-file (string-append tmp "/rm-file3.txt")))
    (define file4 (open-output-file (string-append tmp "/secret.txt")))
    (display "hello" file1)
    (display "hello" file2)
    (display "hello" file3)
    (display "hello" file4)
    (close-output-port file1)
    (close-output-port file2)
    (close-output-port file3)
    (close-output-port file4)
    (define rm (open-file "/bin/rm"))
    (define root (open-dir "/"))
    (rm-test root rm tmpdir (string-append tmp "/rm-file1.txt"))
    (define non-secret-target (open-file (string-append tmp "/rm-file3.txt"))) 
    (rm-nonsecret tmpdir non-secret-target "rm-file3.txt")
    (define secret-target (open-file (string-append tmp "/secret.txt"))) 
    (rm-nonsecret tmpdir secret-target "secret.txt"))
  (require 'ambient))
 "cap"
 "insufficient privileges for unlink!")

(test-pass
 "Successful rename"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rename-test (-> (dir/c #:rename-from (list "rename-from" #t)
                                                   #:rename-to (list "rename-to" #t)
                                                   #:create-dir (list "create-dir" #t (list (list "rename" #t))))
                                            any)]))
    (define (rename-test root) 
      (define target (create-dir root "foo.txt"))
      (rename root target "foo.txt" root "boo.txt")))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "foo~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (rename-test tmpdir))
  (require 'ambient)))

(test-contract-fail
 "Failed rename due to excluded source file"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rename-test (-> (dir/c #:rename-from (list "rename-from" #t (λ (s) (not (string=? s "foo.txt"))))
                                                   #:rename-to (list "rename-to" #t)
                                                   #:create-file (list "create-file" #t (list (list "rename" #t))))
                                            any)]))
    (define (rename-test root) 
      (define target (create-file root "foo.txt"))
      (rename root target "foo.txt" root "boo.txt")))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (rename-test tmpdir))
  (require 'ambient))
 "cap"
 "insufficient privileges for rename!")

(test-contract-fail
 "Failed rename due to excluded source file"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rename-test (-> (dir/c #:rename-from (list "rename-from" #t)
                                                   #:rename-to (list "rename-to" #t (λ (s) (not (string=? s "boo.txt"))))
                                                   #:create-dir (list "create-dir" #t (list (list "rename" #t))))
                                            any)]))
    (define (rename-test root) 
      (define target (create-dir root "foo.txt"))
      (rename root target "foo.txt" root "boo.txt")))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (rename-test tmpdir))
  (require 'ambient))
 "cap"
 "insufficient privileges for rename!")

(test-pass
 "Successful creation and use of one read-only pipe-end and one write-only pipe-end"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [pipe-factory-test (-> (pipe-factory/c #:first (list (list "write" #t))
                                                                  #:second (list (list "read" #t)))
                                                  any)]))
    (define (pipe-factory-test pf) 
      (define pipes (create-pipe pf))
      (write (first pipes)  (string->bytes/utf-8 "hoho"))
      (read (second pipes))))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (pipe-factory-test pipe-factory))
  (require 'ambient)))

(test-contract-fail
 "Failed append on a write-only pipe-end "
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [pipe-factory-test (-> (pipe-factory/c #:first (list (list "write" #t))
                                                                  #:second (list (list "read" #t)))
                                                  any)]))
    (define (pipe-factory-test pf) 
      (define pipes (create-pipe pf))
      (append (first pipes)  (string->bytes/utf-8 "hoho"))
      (read (second pipes))))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (pipe-factory-test pipe-factory))
  (require 'ambient))
 "cap"
 "insufficient privileges for append!")

(test-contract-fail
 "Failed use of no privileges pipe-end created from pipe-factory that produces unusbale pipe-ends"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [pipe-factory-test (-> (pipe-factory/c)
                                                  any)]))
    (define (pipe-factory-test pf) 
      (define pipes (create-pipe pf))
      (append (first pipes)  (string->bytes/utf-8 "hoho"))
      (read (second pipes))))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (pipe-factory-test pipe-factory))
  (require 'ambient))
 "cap"
 "insufficient privileges for append!")

(test-contract-fail
 "Failed use of no privileges pipe-end created from pipe-factory that produces unusbale pipe-ends"
 (script 
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [pipe-factory-test (-> (pipe-factory/c #:second (list (list "read" #t)))
                                                  any)]))
    (define (pipe-factory-test pf) 
      (define pipes (create-pipe pf))
      (append (first pipes)  (string->bytes/utf-8 "hoho"))
      (read (second pipes))))
  (module ambient racket
    (require shill/private/filesystem 'cap)
    (pipe-factory-test pipe-factory))
  (require 'ambient))
 "cap"
 "insufficient privileges for append!")


(test-contract-fail
 "Run rm -- incompatible privileges (file)"
 (script 
  (module cap-wrap racket
    (require shill/private/filesystem)
    (provide (contract-out [wrapper (-> (file/c #:exec (list "exec" #t)
                                                #:read (list "read" #t))
                                        any/c)]))
					 
    (define (wrapper f) f))
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rm-test (-> (dir/c #:contents (list "contents" #t)
                                               #:lookup (list "lookup" #t)
                                               #:read-symlink (list "read-symlink" #t)
                                               #:read (list "read" #t)
                                               #:exec (list "exec" #t)
                                               #:stat (list "stat" #t))
                                        (file/c #:exec (list "exec" #t)
                                                #:read (list "read" #t)
                                                #:write (list "write" #t))
                                        (dir/c ;#:lookup (list "lookup" #t)
					       #:unlink-file (list "unlink-file" #t))
                                        path-string?
                                        any)]))
    (define (rm-test root rm rm-in path)
      (exec rm #:extra (list root rm rm-in) (list "/bin/rm" path))))
  (module ambient racket
    (require shill/private/filesystem 'cap 'cap-wrap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define file1 (open-output-file (string-append tmp "/rm-file1.txt")))
    (define file2 (open-output-file (string-append tmp "/rm-file2.txt")))
    (define file3 (open-output-file (string-append tmp "/rm-file3.txt")))
    (define file4 (open-output-file (string-append tmp "/secret.txt")))
    (display "hello" file1)
    (display "hello" file2)
    (display "hello" file3)
    (display "hello" file4)
    (close-output-port file1)
    (close-output-port file2)
    (close-output-port file3)
    (close-output-port file4)
    (define rm (wrapper (open-file "/bin/rm")))
    (define root (open-dir "/"))
    (rm-test root rm tmpdir (string-append tmp "/rm-file1.txt")))
  (require 'ambient))
 "ambient"
 "expected file with at least +exec, +write, +read unconstrained privileges given #<shill-file> with +exec, +read unconstrained privileges")

(test-contract-fail
 "Run rm -- incompatible privileges (directory)"
 (script 
  (module cap-wrap racket
    (require shill/private/filesystem)
    (provide (contract-out [wrapper (-> (dir/c #:contents (list "contents" #t)
                                                     #:write (list "write" #t))
                                        any/c)]))
					 
    (define (wrapper d) d))
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rm-test (-> (dir/c #:contents (list "contents" #t)
                                               #:lookup (list "lookup" #t)
                                               #:read-symlink (list "read-symlink" #t)
                                               #:read (list "read" #t)
                                               #:exec (list "exec" #t)
                                               #:stat (list "stat" #t))
                                        (file/c #:exec (list "exec" #t)
                                                #:read (list "read" #t)
                                                #:write (list "write" #t))
                                        (dir/c ;#:lookup (list "lookup" #t)
					       #:unlink-file (list "unlink-file" #t))
                                        path-string?
                                        any)]))
    (define (rm-test root rm rm-in path)
      (exec rm #:extra (list root rm rm-in) (list "/bin/rm" path))))
  (module ambient racket
    (require shill/private/filesystem 'cap 'cap-wrap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define file1 (open-output-file (string-append tmp "/rm-file1.txt")))
    (define file2 (open-output-file (string-append tmp "/rm-file2.txt")))
    (define file3 (open-output-file (string-append tmp "/rm-file3.txt")))
    (define file4 (open-output-file (string-append tmp "/secret.txt")))
    (display "hello" file1)
    (display "hello" file2)
    (display "hello" file3)
    (display "hello" file4)
    (close-output-port file1)
    (close-output-port file2)
    (close-output-port file3)
    (close-output-port file4)
    (define rm  (open-file "/bin/rm"))
    (define root (wrapper (open-dir "/")))
    (rm-test root rm tmpdir (string-append tmp "/rm-file1.txt")))
  (require 'ambient))
 "ambient"
 "expected directory with at least +stat, +exec, +read, +lookup, +read-symlink, +contents unconstrained privileges given #<shill-dir> with +write, +contents unconstrained privileges")

(test-contract-fail
 "Run rm -- incompatible privileges (pipe-end)"
 (script 
  (module cap-wrap racket
    (require shill/private/filesystem)
    (provide (contract-out [wrapper (-> (pipe-end/c #:read (list "read" #t))
                                        any/c)]))
					 
    (define (wrapper d) d))
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rm-test (-> (dir/c #:contents (list "contents" #t)
                                               #:lookup (list "lookup" #t)
                                               #:read-symlink (list "read-symlink" #t)
                                               #:read (list "read" #t)
                                               #:exec (list "exec" #t)
                                               #:stat (list "stat" #t))
                                        (pipe-end/c #:read (list "read" #t)
                                                    #:write (list "write" #t)) 
                                        (file/c #:exec (list "exec" #t)
                                                #:read (list "read" #t)
                                                #:write (list "write" #t))
                                        (dir/c ;#:lookup (list "lookup" #t)
					       #:unlink-file (list "unlink-file" #t))
                                        path-string?
                                        any)]))
    (define (rm-test root out rm rm-in path)
      (exec rm #:stdout out #:extra (list root rm rm-in) (list "/bin/rm" path))))
  (module ambient racket
    (require shill/private/filesystem 'cap 'cap-wrap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define file1 (open-output-file (string-append tmp "/rm-file1.txt")))
    (define file2 (open-output-file (string-append tmp "/rm-file2.txt")))
    (define file3 (open-output-file (string-append tmp "/rm-file3.txt")))
    (define file4 (open-output-file (string-append tmp "/secret.txt")))
    (display "hello" file1)
    (display "hello" file2)
    (display "hello" file3)
    (display "hello" file4)
    (close-output-port file1)
    (close-output-port file2)
    (close-output-port file3)
    (close-output-port file4)
    (define rm  (open-file "/bin/rm"))
    (define root (open-dir "/"))
    (rm-test root (wrapper stdout) rm tmpdir (string-append tmp "/rm-file1.txt")))
  (require 'ambient))
 "ambient"
 "expected file with at least +write, +read unconstrained privileges given #<shill-file> with +read unconstrained privileges")

(test-contract-fail
 "Run rm -- incompatible privileges (pipe-factory)"
 (script 
  (module cap-wrap racket
    (require shill/private/filesystem)
    (provide (contract-out [wrapper (-> (pipe-factory/c #:first (list (list "read" #t)) #:second (list (list "write" #t)))
                                        any/c)]))
					 
    (define (wrapper d) d))
  (module cap racket
    (require shill/private/filesystem)
    (provide (contract-out [rm-test (-> (dir/c #:contents (list "contents" #t)
                                               #:lookup (list "lookup" #t)
                                               #:read-symlink (list "read-symlink" #t)
                                               #:read (list "read" #t)
                                               #:exec (list "exec" #t)
                                               #:stat (list "stat" #t))
                                        (pipe-factory/c #:first (list (list "read" #t) (list "write" #t))
                                                        #:second (list (list "read" #t) (list "write" #t))) 
                                        (file/c #:exec (list "exec" #t)
                                                #:read (list "read" #t)
                                                #:write (list "write" #t))
                                        (dir/c ;#:lookup (list "lookup" #t)
					       #:unlink-file (list "unlink-file" #t))
                                        path-string?
                                        any)]))
    (define (rm-test root pipe-fact rm rm-in path)
      (exec rm #:extra (list root rm rm-in pipe-fact) (list "/bin/rm" path))))
  (module ambient racket
    (require shill/private/filesystem 'cap 'cap-wrap)
    (define tmp (path->string (make-temporary-file "rkttmp~a" 'directory #f)))
    (define tmpdir (open-dir tmp))
    (define file1 (open-output-file (string-append tmp "/rm-file1.txt")))
    (define file2 (open-output-file (string-append tmp "/rm-file2.txt")))
    (define file3 (open-output-file (string-append tmp "/rm-file3.txt")))
    (define file4 (open-output-file (string-append tmp "/secret.txt")))
    (display "hello" file1)
    (display "hello" file2)
    (display "hello" file3)
    (display "hello" file4)
    (close-output-port file1)
    (close-output-port file2)
    (close-output-port file3)
    (close-output-port file4)
    (define rm  (open-file "/bin/rm"))
    (define root (open-dir "/"))
    (define p-f (wrapper pipe-factory))           
    (rm-test root p-f rm tmpdir (string-append tmp "/rm-file1.txt")))
  (require 'ambient))
 "ambient"
 "expected pipe-factory with at least +write, +read and +write, +read unconstrained privileges for the first and second pipe-end respectively given #<shill-pipe-factory> with +read and +write unconstrained privileges for the first and second pipe-end respectively for the first and second pipe-end respectively")