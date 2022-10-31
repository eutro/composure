#lang gdlisp

(class-name TypeClass)
(require "../../macros.rkt")

(splice-record
 ([name : String]))

(define (_to-string) name)
