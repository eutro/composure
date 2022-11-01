#lang racket

(require (prefix-in $ gdlisp)
         (for-syntax
          syntax/parse
          racket/sequence
          racket/syntax))

(provide defrecord splice-record)

(begin-for-syntax
  (define-splicing-syntax-class record-field
    #:attributes (name (prefix 1) (hint 1))
    (pattern name:id #:attr (hint 1) null #:attr (prefix 1) null)
    (pattern [name:id hint ...] #:attr (prefix 1) null)
    (pattern {~seq #:export (xprt ...) tl:record-field}
             #:attr name #'tl.name
             #:attr (hint 1) (syntax->list #'(tl.hint ...))
             #:attr (prefix 1) (syntax->list #'(($export xprt ...) tl.prefix ...)))))

(define-syntax (splice-record stx)
  (syntax-parse stx
    [(_ (field:record-field ...))
     (with-syntax ([(init-arg ...)
                    (for/list ([ag (in-syntax #'(field.name ...))])
                      (format-id ag "~a-in" ag))])
       (syntax/loc stx
         ($begin
           ($define field.prefix ... field.name field.hint ...) ...
           ($define (_init init-arg ...)
             ($set! field.name init-arg) ...))))]))

(define-syntax (defrecord stx)
  (syntax-parse stx
    [(_ name:id (arg ...)
        body ...)
     (syntax/loc stx
       ($class name
         (splice-record (arg ...))
         body ...))]))
