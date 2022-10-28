#lang gdlisp

(class-name InputKeyMouse)

(extends InputKeyBase)

(define event : InputEventMouse)

(define (_init evt)
  (set! event evt))

(define (_map-key)
  ["Mouse" (.-button-mask event)])
