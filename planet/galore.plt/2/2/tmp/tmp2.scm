(require "list-stack.scm"
         (lib "42.ss" "srfi"))

(define-syntax stack-ec
  (syntax-rules ()
    [(stack-ec etc1 etc ...)
     (fold-ec empty etc1 etc ... insert)]))

(define (stack-dispatch args)
  (cond
    [(null? args)
     'stack]
    [(and (= (length args) 1)
          (stack? (car args)))
     (:generator-proc (:list (elements (car args))))]
    [else
     #f]))



(define s (stack-ec (: i 5) i))

(list-ec (: x (insert* (list 1 2 3 4) empty))
           x)