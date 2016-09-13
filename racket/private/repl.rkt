#lang racket/base

(require shill/private/filesystem)

(provide (all-defined-out))

(define-values (replout-in stdout)
  (apply values (create-pipe pipe-factory)))

(define-values (replerr-in stderr)
  (apply values (create-pipe pipe-factory)))
