#lang racket

(require rackunit)
(require shill/private/ffi shill/private/sandbox)
(define tmp (make-temporary-file "rkttmp~a" 'directory #f))

(test-case
 "Run ls"
 (define ls (open-for-exec (string->path "/bin/ls")))
 (define root (open-dir tmp))
 (define in-name (string->path "in.txt"))
 (define out-name (string->path "out.txt"))
 (create-file root in-name)
 (create-file root out-name)
 (define in (lookup-file root in-name))
 (define out (lookup-file root out-name))
 (define-values (ls-files ls-caps)
   (let* ([fsroot (open-dir (string->path "/"))]
          [libmap (open-file (string->path "/etc/libmap.conf"))]
          [ld-elf (open-file (string->path "/libexec/ld-elf.so.1"))]
          [libc (open-file (string->path "/lib/libc.so.7"))]
          [libutil (open-file (string->path "/lib/libutil.so.9"))]
          [libncurses (open-file (string->path "/lib/libncurses.so.8"))]
          [locale1 (open-file (string->path "/usr/share/locale/en_US.UTF-8"))]
          [locale2 (open-file (string->path "/usr/share/locale/UTF-8/LC_CTYPE"))]
          [ld-elf-hints (open-file (string->path "/var/run/ld-elf.so.hints"))]
          [contentread (list 'contents 'read)]
          [lookupstat (list (list 'lookup #f) 'stat)]
          [read (list 'read)]
          [write (list 'write 'append)]
          [readexec (list 'read 'exec)]
          [files (list ls in out root fsroot libmap ld-elf libc libutil libncurses locale1 locale2 ld-elf-hints)]
          [caps (list readexec read write contentread lookupstat read readexec readexec readexec readexec read read read)])
     (values files caps)))
 (shill-sandbox ls in out out ls-files ls-caps empty empty (list "/bin/ls" (path->string tmp)) #f)
 (display (bytes->string/utf-8 (read-file out)))
 (close-node in)
 (close-node out)
 (close-node root)
 (close-node ls))

(delete-directory/files tmp)
