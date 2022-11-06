#lang gdlisp

(class-name InputKeyKeyboard)
(require "../../macros.rkt")

(extends InputKeyBase)

(splice-record ([event : InputEventKey]))

(define (_map-key)
  ["Key" (float (.get-scancode-with-modifiers event))])

(define (_scene)
  (preload "KeyboardDisplay.tscn"))

(define (get-lock-type)
  (.-pressed event))

(define (_lambda-type)
  Types.TY_UNIT)

(define (resolve-value)
  Values.VAL_UNIT)
