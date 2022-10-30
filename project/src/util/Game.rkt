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
          [["Vector" Values.VAL_VEC]
           ["Compose" Values.VAL_COMPOSE]
           ["ComposeA" Values.VAL_COMPOSEA]
           ["Constant" Values.VAL_K]
           ["Identity" Values.VAL_I]
           ["Ap" Values.VAL_S]
           ["Corrupt" Values.VAL_CORRUPT]
           ["Move" Values.VAL_MOVE]
           ["Prn" Values.VAL_PRN]])))

(signal copy-source-changed)
(define copy-src)
(define (set-copy-source src)
  (set! copy-src src)
  (emit-signal "copy_source_changed")
  null)
