#lang gdlisp

(extends KinematicBody)

(define onready at $AnimationTree)

(define MAX_SPEED 2)

(define velocity : Vector3 (Vector3 0 0 0))

(define (user-move vel)
  (set! velocity vel)
  (set! (.-y velocity) 0)
  (define mag (.length velocity))
  (when (> mag MAX_SPEED)
    (*set! velocity (/ MAX_SPEED mag))))

(define (_physics-process delta)
  (define speed (.length velocity))
  (cond
    [(> speed 0)
     (set! (ref at "parameters/Motion/blend_amount")
           (/ speed MAX_SPEED))
     (look-at (+ translation velocity) Vector3.UP)
     (move-and-slide velocity)]
    [else
     (set! (ref at "parameters/Motion/blend_amount") 0)])
  null)
