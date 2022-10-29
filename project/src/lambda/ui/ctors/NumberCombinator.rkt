#lang gdlisp

(class-name NumberCombinator)
(extends Control)

(define onready slot $HBox/Slot)

(define (set-value value)
  (.set-term slot (.new LambdaNumber value)))

(define (_on_Number_value_changed value)
  (set-value value))

(define (_ready)
  (set-value (.-value $HBox/Number))
  null)
