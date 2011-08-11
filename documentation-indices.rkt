#lang racket/base
(provide (all-defined-out))
  
(require "config.scm"
         "indexer.scm")

(define the-sensitive-index   (load-index (name->index-path "sensitive")))
(define the-insensitive-index (load-index (name->index-path "insensitive")))
