#lang gdlisp

(class-name TypeCtor)

(define name : String)
(define arity : int)
(define infix : bool)

(define (_init nm art infx)
  (set! name nm)
  (set! arity art)
  (set! infix infx))
