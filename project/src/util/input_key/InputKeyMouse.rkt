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
  Types.TY_VEC)

(define (resolve-value)
  ;; TODO
  (.new LambdaWrapper (Vector3 0 0 0) Types.TY_VEC))
