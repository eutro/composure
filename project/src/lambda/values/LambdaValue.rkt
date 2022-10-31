#lang gdlisp

(class-name LambdaValue)

(define (get-type) : Type
  null)

(define (create-preview) : Control
  ;; create a preview for this term
  (.create TextPreview "λ?"))
