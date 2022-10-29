#lang gdlisp

(class-name LambdaValue)

(define (get-type) : LambdaType
  null)

(define (create-preview) : Control
  ;; create a preview for this term
  (.create TextPreview "λ?"))
