#lang gdlisp

(class-name TypeCtor)
(require "../../macros.rkt")

(splice-record
 ([name : String]
  [arity : int]
  [infix : bool]
  [type-classes : Dictionary #;{TypeClass true}]
  [create-preview : FuncRef #;(-> T Control)]))

(define (create-preview x)
  (.call-func create-preview x))
