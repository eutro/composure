#lang gdlisp

(extends Node)

(define keys : KeyBinder (.new KeyBinder))
(define world null)
(define ui null)

(begin-escape
  (require (only-in racket/base define-syntax)
           (for-syntax racket syntax/parse))
  (define-syntax (repeat stx)
    (syntax-parse stx
      [(_ n:integer expr)
       (with-syntax ([(repetitions ...)
                      (for/list ([i (in-range (syntax-e #'n))])
                        #'expr)])
         (syntax/loc stx [repetitions ...]))])))

(define user-defs (repeat 100 ["" null]))

(signal copy-source-changed)
(define copy-src)
(define (set-copy-source src)
  (set! copy-src src)
  (emit-signal "copy_source_changed")
  null)
