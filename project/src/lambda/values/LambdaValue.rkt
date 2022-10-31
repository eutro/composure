#lang gdlisp

(class-name LambdaValue)

(define (get-tooltip)
  (define tt [])
  (_add-tooltip tt)
  tt)

(define (_add-tooltip [lines : Array])
  null)

(define (get-type) : Type
  null)

(define (create-preview) : Control
  ;; create a preview for this term
  (.create TextPreview "λ?"))
