#lang gdlisp

(class-name Result)

(define is-ok : bool)
(define value)

(define (_init ok vl)
  (set! is-ok ok)
  (set! value vl))
