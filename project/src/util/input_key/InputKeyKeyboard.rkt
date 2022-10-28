#lang gdlisp

(class-name InputKeyKeyboard)

(extends InputKeyBase)

(define event : InputEventKey)

(define (_init evt)
  (set! event evt))

(define (_map-key)
  ["Key" (.get-scancode-with-modifiers event)])

(define (_scene)
  (preload "KeyboardDisplay.tscn"))
