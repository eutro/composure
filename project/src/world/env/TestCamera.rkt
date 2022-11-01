#lang gdlisp

(extends Spatial)

(define (_ready)
  (.init $EnvInside))

(define (_physics-process delta)
  (define vec (Input.get-vector "joy_left_right" "joy_left_left" "joy_left_up" "joy_left_down"))
  (+set! $Camera.translation (* delta (Vector3 vec.x 0 vec.y) 10)))
