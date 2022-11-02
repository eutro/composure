#lang gdlisp

(class-name LambdaWrapper)
(extends LambdaValue)

(define value)
(define type : Type)

(define (_init vl ty)
  (set! value vl)
  (set! type ty))

(define (get-type) type)

(define (create-preview)
  (type.mono.ctor.create-preview value))

(define (_to-string)
  (str value))
