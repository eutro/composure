#lang gdlisp

(class-name LambdaWrapper)
(extends LambdaValue)

(define value)
(define type : LambdaType)

(define (_init vl ty)
  (set! value vl)
  (set! type ty))

(define (get-type) type)

(define (create-preview)
  (.create TextPreview (str value)))