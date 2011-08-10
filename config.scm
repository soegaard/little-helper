#lang racket/base
(provide (all-defined-out))

(require setup/dirs
         (only-in mzlib/etc this-expression-source-directory))

(define default-sensitivity        (make-parameter #t))
(define current-data-directory     (make-parameter (build-path (this-expression-source-directory) "data")))
(define current-index-name         (make-parameter "sensitive"))
(define current-lexicon-block-size (make-parameter 100))

; The number of term-doc-frequencies records that
; can be in memory at a time (see indexer.scm).
; Use a low number on memory starved machines.
(define MAX-RECORDS-IN-MEMORY 500000)
;(define MAX-RECORDS-IN-MEMORY 1)

(define the-index-dir           (string->path "/tmp/little-helper/doc"))
(define the-index-path          (build-path the-index-dir "doc.index"))
(define the-index-inverted-path (build-path the-index-dir "doc.inverted"))
(define the-original-repository-path  (build-path "/tmp/docs.racket-lang.org"))
(define the-repository-path     
  ; (find-doc-dir)  ; local machine
  ; fetch new copy with wget, then delete .gif and png. (and html after conversion)
  ; wget -r -D docs.racket-lang.org http://docs.racket-lang.org 
  ; (build-path "/tmp/docs.racket-lang.org")  
  (build-path (current-data-directory) "docs.racket-lang.org")
  )
(define the-repository-txt-path     
  ; (find-doc-dir)  ; local machine
  ; fetch new copy with wget, then delete .gif and png. (and html after conversion)
  ; wget -r -D docs.racket-lang.org http://docs.racket-lang.org 
  (build-path (current-data-directory) "docs.racket-lang.org")
  )


(define (lexicon-path index-name)
  (build-path (current-data-directory) index-name "lexicon.txt"))
(define (lexicon-index-path index-name)
  (build-path (current-data-directory) index-name "lexicon-index.txt"))
(define (inverted-path index-name)
  (build-path (current-data-directory) index-name "inverted.txt"))
(define (name->index-path index-name)
  (build-path (current-data-directory) index-name "index.txt"))

(define (flat-lexicon-path index-name)
  (build-path (current-data-directory) index-name "flat-lexicon.txt"))
