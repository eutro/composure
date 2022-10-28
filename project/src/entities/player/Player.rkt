#lang gdlisp

(extends KinematicBody)

(define onready at $AnimationTree)

(define velocity : Vector3 (Vector3 0 0 0))

(define (_physics-process delta)
  (cond
    [(> (.length-squared velocity) 0)
     (set! (ref at "parameters/Motion/blend_amount") 1)]
    [else
     (set! (ref at "parameters/Motion/blend_amount") 0)])
  (look-at (+ translation velocity) Vector3.UP)
  null)
