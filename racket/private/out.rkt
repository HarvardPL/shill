#lang racket

(require shill/private/filesystem
         shill/private/net)

;from filesystem
(provide 
         ;cap predicates
         dir?
         file?           
         pipe-factory?         
         pipe-end?
         socket-factory?
         sys-error?
         
         ;contracts
         dir/c
         file/c
         pipe-factory/c
         pipe-end/c
         socket-factory/c
         
         ;cap constructors
         open-dir
         open-file
         pipe-factory
         socket-factory
         
         ;cap operations
         path
         contents
         create-dir
         create-file
         create-pipe
         add-link
         add-symlink
         read-symlink
         unlink-file
         unlink-dir
         rename
         lookup
         chdir
         read
         write
         append
         exec
         close
         cwd
         stdin
         stdout
         stderr
	 size
	 accessed
	 modified
	 created
	 status-changed
	 uid
	 gid
	 suid
	 sgid
	 perms

         path-element-string?)
