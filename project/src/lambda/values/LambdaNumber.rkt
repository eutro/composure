#lang gdlisp

(class-name LambdaNumber)
(extends LambdaValue)

(define value : float)

(define (_init vl)
  (set! value vl))

(define (get-type)
  Types.TY_NUM)

(define (create-preview)
  (.create TextPreview (str value)))
