#lang gdlisp

(class-name InputKeyMouse)

(extends InputKeyBase)

(define event : InputEventMouseButton)

(define (_init evt)
  (set! event evt))

(define (_map-key)
  ["Mouse" (.-button-index event)])

(define (_scene)
  (preload "MouseDisplay.tscn"))

(define (_lambda-type)
  Types.TY_VEC2)

(define (get-lock-type)
  (.-pressed event))

(define (resolve-value)
  (Values.wrap-vec2 (.get-mouse-position (Game.get-viewport))))
