(module macro-utilities mzscheme
  (provide with-captures)
  (define-syntax (with-captures stx)
    (syntax-case stx ()
      [(_ so (name ...) body)
       #'(with-syntax
             ([name (datum->syntax-object so 'name)]
              ...)
           body)])))
   
   (module signature mzscheme
     (require-for-syntax macro-utilities)
     (require (lib "contract.ss"))
     (provide define-signature)
     
     (define-syntax (define-signature so)
       (syntax-case so (where)
         [(define-signature provide-name provide-name-no-contracts 
            ((name contract)
             ...))
          #'(define-signature provide-name provide-name-no-contracts
              ((name contract) ...)
              (where))]
         [(define-signature provide-name provide-name-no-contracts
            ((name contract)
             ...)
            (where expr ...))
          #'(begin
              ; with contracts
              (define-syntax (provide-name stx)
                (syntax-case stx ()
                  [(src)
                   (with-captures stx (name ...)
                     (quasisyntax/loc stx
                       (begin
                         expr ...
                         #,((syntax-local-value #'provide/contract)
                            #`(#,(datum->syntax-object (syntax src) 'provide/contract)
                                 (#,(datum->syntax-object (syntax src) 'name) contract)
                                 ...)))))]))
              ; no contracts
              (define-syntax (provide-name-no-contracts stx)
                (syntax-case stx ()
                  [(_)
                   (with-captures stx (name ...)
                     #'(provide name ...))])))])))
 
 (module stack-signature mzscheme
   (provide provide-stack )
   
   (require ;signature
    (lib "contract.ss"))
   (require-for-syntax macro-utilities)
   
   #;
   (define-syntax (define-signature so)
     (syntax-case so (where)
       [(define-signature provide-name provide-name-no-contracts 
          ((name contract)
           ...))
        #'(define-signature provide-name provide-name-no-contracts
            ((name contract) ...)
            (where))]
       [(define-signature provide-name provide-name-no-contracts
          ((name contract)
           ...)
          (where expr ...))
        #'(begin
            ; with contracts
            (define-syntax (provide-name stx)
              (syntax-case stx ()
                [(src)
                 (with-captures stx (name ...)
                   (quasisyntax/loc stx
                     (begin
                       expr ...
                       #,((syntax-local-value #'provide/contract)
                          #`(provide/contract
                             (name contract)
                             ...)))))]))
            ; no contracts
            (define-syntax (provide-name-no-contracts stx)
              (syntax-case stx ()
                [(_)
                 (with-captures stx (name ...)
                   #'(provide name ...))])))]))
   
   (define-syntax (define-signature so)
     (syntax-case so (where)
       [(define-signature provide-name provide-name-no-contracts
          ((name contract)
           ...)
          (where expr ...))
        #'(define-syntax (provide-name stx)
            (syntax-case stx ()
              [(src)
               (with-syntax ([pvd
                              (datum->syntax-object
                               stx
                               (syntax-e
                                (with-captures stx (provide-contract name ...)
                                  (syntax/loc stx
                                    (provide/contract
                                     (name contract)
                                     ...)))))])
                 (with-captures stx (provide-contract name ...)
                   (quasisyntax/loc stx
                     (begin
                       expr ...
                       pvd))))]))]))
   
   (define-signature provide-stack provide-stack-no-contracts
     ((empty  stack/c)
      (head   (-> stack/c          any/c))
      (push   (-> any/c stack/c   stack/c))
      (pop    (-> stack/c          any/c))
      (stack? (-> any/c            boolean/c)))
     (where
      (define stack/c   (flat-named-contract 'stack stack?))
      (define boolean/c (flat-named-contract 'boolean boolean?)))))
 
 
 ;         (define-syntax (provide-stack stx)
 ;           (syntax-case stx ()
 ;             [(_)
 ;              (with-captures stx
 ;                (empty head pop push stack?)
 ;                (quasisyntax/loc stx
 ;                  (begin
 ;                    (require (lib "contract.ss"))
 ;                    (define stack/c
 ;                      (flat-named-contract 'stack stack?))
 ;                    (define boolean/c
 ;                      (flat-named-contract 'boolean boolean?))
 ;                    #,((syntax-local-value #'provide/contract)
 ;                       #'(provide/contract
 ;                          (empty  stack/c)
 ;                          (head   (-> stack/c          any/c))
 ;                          (push   (-> any/c stack/c   stack/c))
 ;                          (pop    (-> stack/c          any/c))
 ;                          (stack? (-> any/c            boolean/c)))))))]))
 ;         
 ;         )
 
 
 
 (module simple-stack mzscheme
   (require stack-signature)
   
   (define empty  '())
   (define (stack? o)
     (or (null? o) (pair? o)))
   (define push cons)
   (define pop    cdr)
   (define head   car)
   
   (provide-stack)
   )
 
 (require simple-stack)
 (push 42 empty)
 
 (module better-stack mzscheme
   (require stack-signature
            (prefix simple: simple-stack))
   (define-struct stack (simple-stack) (make-inspector))
   (define empty (make-stack simple:empty))
   (define (head s)
     (simple:head (stack-simple-stack s)))
   (define (push x s)
     (make-stack (simple:push x (stack-simple-stack s))))
   (define (pop s)
     (simple:pop (stack-simple-stack s)))
   
   (provide-stack))
 
 
 (print-struct #t)
 (require better-stack)
 (push 42 empty)
 
 (module super-stack mzscheme
   (require stack-signature
            (prefix better: better-stack))
   (define-struct stack (size better-stack) (make-inspector))
   (define empty (make-stack 0 better:empty))
   (define (head s)
     (better:head (stack-better-stack s)))
   (define (push x s)
     (make-stack (add1 (size s))
                 (better:push x (stack-better-stack s))))
   (define (pop s)
     (make-stack (sub1 (size s))
                 (better:pop (stack-better-stack s))))
   (define (size s)
     (stack-size s))
   
   ;(provide-stack-no-contracts)
   (provide-stack)
   (provide size))
 
 (require super-stack)
 (require (prefix better: better-stack))
 ; (push 42 43)
 
 