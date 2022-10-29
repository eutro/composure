#lang gdlisp

(extends HBoxContainer)

(define (export bool) editable true)

(define onready name-box $Name)
(define onready value $Value)

(define entry)

(define (_ready)
  (set! (.-editable name-box) editable)
  (set! (.-editable value) editable)
  (set! (.-remove-on-drag value) false)
  (when entry
    (.set-text name-box (ref entry 0))
    (.set-term value (ref entry 1)))
  null)
