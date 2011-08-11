#lang racket/base
(provide (all-defined-out))
  
(require "config.rkt"
         "indexer.rkt")

(define the-sensitive-index   (load-index (name->index-path "sensitive")))
(define the-insensitive-index (load-index (name->index-path "insensitive")))
