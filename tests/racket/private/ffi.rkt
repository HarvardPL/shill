#lang racket


(require rackunit shill/private/ffi)

(define tmp (make-temporary-file "rkttmp~a" 'directory #f))

(test-case
 "Create, read, and write a file"
 (define msg "Hello, world!")
 (define root (open-dir tmp))
 (create-file root (string->path-element "tmp.txt"))
 (define txt (lookup-file root (string->path-element "tmp.txt")))
 (write-file txt (string->bytes/utf-8 msg))
 (close-node txt)
 (define txt2 (lookup-file root (string->path-element "tmp.txt")))
 (check-equal? (bytes->string/utf-8 (read-file txt2)) msg)
 (close-node txt2)
 (define todel (lookup-file root (string->path-element "tmp.txt")))
 (unlink-file root (string->path-element "tmp.txt") todel)
 (check-true (not (desc? (lookup-file root (string->path-element "tmp.txt")))))
 (close-node todel)
 (close-node root))

(test-case
 "List the contents of a directory"
 (define root (open-dir tmp))
 (create-file root (string->path-element "file1.txt"))
 (create-file root (string->path-element "file2.txt"))
 (define content (contents root))
 (define files (list "file1.txt" "file2.txt"))
 (check-equal? content files)
 (close-node root))

(test-case
 "Pipes"
 (define ends (new-pipe))
 (define in (first ends))
 (define out (second ends))
 (check-pred desc? in "In end of pipe should be a file")
 (check-pred desc? out "Out end of pipe should be a file")
 (write-pipe out (string->bytes/utf-8 "hello"))
 (close-node out)
 (check-equal? (bytes->string/utf-8 (read-pipe in)) "hello")
 (close-node in))

(test-case
 "Path"
 (define root (open-dir tmp))
 (check-equal? tmp (path root)))
