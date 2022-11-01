#lang gdlisp

(extends Node)

(define keys : KeyBinder (.new KeyBinder))
(define world null)
(define ui null)

(define user-defs
  [["" null]
   ["" null]
   ["" null]
   ["" null]
   ["" null]])

(signal copy-source-changed)
(define copy-src)
(define (set-copy-source src)
  (set! copy-src src)
  (emit-signal "copy_source_changed")
  null)
