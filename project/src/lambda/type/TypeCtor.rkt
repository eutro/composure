#lang gdlisp

(class-name TypeCtor)
(require "../../macros.rkt")

(splice-record
 ([name : String]
  [arity : int]
  [infix : bool]
  [type-classes : Dictionary #;{TypeClass true}]))
