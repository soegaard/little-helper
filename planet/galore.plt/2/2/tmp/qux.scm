(module qux1 mzscheme
  (require (lib "contract.ss"))
  
  (define (concat l1 l2)
    (append l1 l2))
  
  (provide/contract
   (concat (-> pair? pair?  pair?))))

(module sig mzscheme
  (require-for-syntax "macro-utilities.scm")
  (provide provide-concat)
  (define-syntax (provide-concat stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (concat)
         (syntax/loc stx 
           (begin
             (require (lib "contract.ss"))
             (provide/contract
              (concat (-> pair? pair?  pair?))))))])))

(module qux2 mzscheme
  (require sig)

  (define-syntax (define-alias stx)
    (syntax-case stx ()
      [(_ new-name old-name)
       (syntax/loc stx
         (define (new-name l1 l2)
           (old-name l1 l2)))]))

  (define-alias concat append)
  
  (provide-concat))


(module bar mzscheme
  (require qux2)
  (display (concat (list 1 2) (vector 3 4))))

(require bar)
