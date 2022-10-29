#lang gdlisp

(extends Node)

(define keys : KeyBinder (.new KeyBinder))
(define world null)

(define builtin-defs)
(define user-defs
  [["" null]
   ["" null]
   ["" null]
   ["" null]
   ["" null]])

(define (init-defs)
  (when (== null builtin-defs)
    (set! builtin-defs
          [["add" Values.VAL_ADD]
           ["compose" Values.VAL_COMPOSE]
           ["id" Values.VAL_I]
           ["S" Values.VAL_S]])))

(signal copy-source-changed)
(define copy-src)
(define (set-copy-source src)
  (set! copy-src src)
  (emit-signal "copy_source_changed")
  null)
