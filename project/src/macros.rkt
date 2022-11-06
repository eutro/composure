#lang racket

(require (prefix-in $ gdlisp)
         threading
         (for-syntax
          syntax/parse
          racket/sequence
          racket/syntax))

(provide doto defrecord splice-record
         begin-ready! finish-ready! do-onready)

(begin-for-syntax
  (define all-ready (box null)))

(define-syntax (begin-ready! stx)
  (set-box! all-ready null)
  #'($begin))

(define-syntax (finish-ready! stx)
  (quasisyntax/loc stx
    ($define (_ready)
      #,@(reverse (unbox all-ready))
      null)))

(define-syntax (do-onready stx)
  (syntax-parse stx
    [(_ body:expr ...)
     (set-box! all-ready (cons #'($begin body ...) (unbox all-ready)))
     #'($begin)]))

(begin-for-syntax
  (define-splicing-syntax-class record-field
    #:attributes (name (prefix 1) (hint 1))
    (pattern name:id #:attr (hint 1) null #:attr (prefix 1) null)
    (pattern [name:id hint ...] #:attr (prefix 1) null)
    (pattern {~seq #:export (xprt ...) tl:record-field}
             #:attr name #'tl.name
             #:attr (hint 1) (syntax->list #'(tl.hint ...))
             #:attr (prefix 1) (syntax->list #'(($export xprt ...) tl.prefix ...)))))

(define-syntax (doto stx)
  (syntax-parse stx
    [(_ x c ...)
     (with-syntax ([tmp (gensym)])
       (syntax/loc stx
         ($let ([tmp x])
           (~> tmp c) ...
           tmp)))]))

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
