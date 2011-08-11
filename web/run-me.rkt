#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/templates
         racket/runtime-path
         (prefix-in xml: xml)
         "../documentation-indices.rkt"
         "../query.rkt"
         "../snippet.rkt"
         "../indexer.rkt"
         "../config.rkt")

;;; CONTROL

(define-values (dispatch page-url)
  (dispatch-rules
   [("view" (string-arg) ...) view-page]
   [else                      search-page]))
  
(define (search-page request)
  (response/full 
   200 #"Okay" (current-seconds) TEXT/HTML-MIME-TYPE empty
   (list 
    (string->bytes/utf-8 
     (generate-search-page request)))))

(define (view-page request . args)
  (response/full 
   200 #"Okay" (current-seconds) TEXT/HTML-MIME-TYPE empty
   (list 
    (string->bytes/utf-8 
     (generate-search-page request)))))

(define (get-query-string request)
  (match (bindings-assq #"q" (request-bindings/raw request))
    [(? binding:form? b)
     (bytes->string/utf-8 (binding:form-value b))]
    [_ #f]))

(define (get-query-regexp request)
  (match (bindings-assq #"qr" (request-bindings/raw request))
    [(? binding:form? b)
     (bytes->string/utf-8 (binding:form-value b))]
    [_ #f]))

(define (make-checkbox-getter name)
  (lambda (request)
    (match (bindings-assq name (request-bindings/raw request))
      [(? binding:form? b)
       (equal? "on" (bytes->string/utf-8 (binding:form-value b)))]
      [_ #f])))

(define get-sensitivity    (make-checkbox-getter #"s"))
(define get-contain-all    (make-checkbox-getter #"ca"))

(define (get-normal request)
  (match (bindings-assq #"n" (request-bindings/raw request))
    [(? binding:form? b)
     (begin
       (display (bytes->string/utf-8 (binding:form-value b)))
       (newline)
       (equal? "on" (bytes->string/utf-8 (binding:form-value b))))]
    [_ #f]))

;;; UTILITIES

(define (intersperse x ls)
  (add-between ls x))

(define (generate-search-page request)
  (define (checked-string v)
    (if v "checked='on'" ""))
  (let* ([q      (get-query-string request)] ; q = #f  <=> no query string
         [html   string-append]              ; prettier template
         [checked-s  (checked-string (get-sensitivity request))]
         [checked-ca (checked-string (get-contain-all request))]
         [checked-n  (if (get-normal request) "on" "")]
         [result (when q
                   (display (list 'Options 
                                  'sensitive:   (get-sensitivity request) 
                                  'contain-all: (get-contain-all request)
                                  'normal       (get-normal request)))
                   (newline)
                   (xml:xexpr->string 
                    (query->xexpr q
                                  (get-sensitivity request) 
                                  (get-contain-all request)
                                  (get-normal request))))])
    (include-template "search.html")))

(define (query->xexpr q sensitive contain-all type-normal)
  (let* ([the-index (if sensitive the-sensitive-index the-insensitive-index)])
    (let-values ([(hits terms)
                  (query the-index q sensitive contain-all type-normal)])
      ; hits is a list of (cons d score)
      `(div ([class "results"])
            (div ([class "info"])
                 "Total number of hits: " ,(number->string (length hits)))
            (br)
            ,(hits->xml the-index hits terms sensitive)))))

(define hit-document car)
(define hit-score cdr)

(define (hits->xml index hits terms sensitive?)
  (let* ([bars (hits->progressbars hits)])
  `(div ([class "hits"])
        ,@(for/list ([hit hits]
                     [bar bars])
              (hit->xml index hit terms sensitive? bar)))))

(define (hits->progressbars hits)
  (percentages->progressbars
   (hits->percentages hits)))

(define (hits->percentages hits)
  (scores->percentages (map hit-score hits)))

(define (scores->percentages scores)
  (if (null? scores)
      '()
      (let ([highest (apply max scores)])
        (map (λ (x) (inexact->exact (ceiling (* 100 (/ x highest))))) scores))))

(define (percentages->progressbars percentages)
  (for/list ([p percentages]
             [i (in-naturals)])
      (format "<div id='progressbar_~a' class='progressbar' pct='~a'></div>" i p)))

(define (hit->xml index h terms sensitive? bar)
  (let* ([d       (hit-document h)]
         [score   (hit-score h)]
         [snippet (document->snippet 
                   (build-path the-snippet-repository-path (document-number->source-path index d))
                   terms sensitive?)])
    (xml:empty-tag-shorthand 'never) ; due to jQuery
    (let ([url (document-number->url index d)])
      `(div ([class "hit"])
            (p ,(xml:string->xexpr bar) " "
               (a ([href ,url]) 
                  ,(relative-path->link-text
                    (document-number->source-path index d))))
            ,@(if (not snippet)
                  '()
                  (list `(tt ,@(intersperse '(br) snippet))))))))

(define (document->snippet file terms sensitive?)
  ; file is given with absolute path
  ; Find snippet with the first term occuring in the document.
  ; TODO: An improvement would be to find a snippet containing
  ;       multiple terms, rather than just the first.
  (let ([file (if (equal? (filename-extension file) #"html")
                  (path-add-suffix file ".txt")
                  file)])
    (define (get-snippet-for-term term)
      (let ([ls (occurs-at-lines file term sensitive?)])
        (cond [(null? ls) #f]
              [else 
               (snippet-at-line file (car ls) 1)])))
    (define (get-snippet-from-a-term terms)
      (ormap get-snippet-for-term terms))
    (get-snippet-from-a-term terms)))


(define (document-number->source-path index d)
  (lookup-document-path index d))

(define (document-number->url index d)
  (cond
    [(document-number->source-path index d)
     => (λ (relative-path)
          (apply string-append
                 "http://docs.racket-lang.org/"
                 (intersperse 
                  "/"
                  (map path->string
                       (explode-path relative-path)))))]
    [else
     #f]))

(define (relative-path->link-text relative-path)
  (apply string-append
         (intersperse 
          "/ "
          (map path->string
               (explode-path 
                relative-path)))))

(define (absolute-path->link-text full-path)
  (relative-path->link-text
   (find-relative-path the-repository-path full-path)))

(define-runtime-path css-path "css")
(define-runtime-path root-path "web-root")
(display root-path) (newline)
(display css-path) (newline)

(serve/servlet dispatch
               #:servlet-path ""
               #:servlet-regexp #rx"^(?!(/css/))"
               #:server-root-path root-path
               #:extra-files-paths (list css-path))
