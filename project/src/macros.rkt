#lang racket

(require (prefix-in $ gdlisp)
         (for-syntax
          syntax/parse
          racket/sequence
          racket/syntax))

(provide defrecord splice-record)

(begin-for-syntax
  (define-syntax-class record-field
    #:attributes (name (hint 1))
    (pattern name:id #:attr (hint 1) null)
    (pattern [name:id hint ...])))

(define-syntax (splice-record stx)
  (syntax-parse stx
    [(_ (field:record-field ...))
     (with-syntax ([(init-arg ...)
                    (for/list ([ag (in-syntax #'(field.name ...))])
                      (format-id ag "~a-in" ag))])
       (syntax/loc stx
         ($begin
           ($define field.name field.hint ...) ...
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
