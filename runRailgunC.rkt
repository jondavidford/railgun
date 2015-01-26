#lang racket
(require "railgun.rkt")

(define (railgun-file)
  (define file-string (vector-ref (current-command-line-arguments) 0))
  (define in-file (open-input-file file-string))
  (define prog (read in-file))
  (display (compile prog)))

(railgun-file)

