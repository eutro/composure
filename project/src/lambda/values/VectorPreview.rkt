#lang gdlisp

(class-name VectorPreview)
(extends Control)

(define const plane-norm (Vector3 0.5 0.707 0.5))
(define const plane-up (Vector3 -0.5 0.707 -0.5))
(define const plane-right (Vector3 0.707 0 -0.707))

(define static (create vec)
  (define inst (.instance (load "res://src/lambda/values/VectorPreview.tscn")))
  (define is-2d (is vec Vector2))
  (define is-zero (== 0 (.length-squared vec)))
  (define node
    (inst.get-node
     (cond
       [is-zero
        (if is-2d "Z2D" "Z3D")]
       [is-2d "Vc2D"]
       [else (if ((.dot vec plane-norm) . > . 0) "Fw3D" "Bk3D")])))
  (.show node)
  (when (not is-zero)
    (define proj-vec
      (.normalized
       (if is-2d
           vec
           (Vector2 (.dot plane-right vec)
                    (.dot plane-up vec)))))
    (define angle (- (Vector2.RIGHT.angle-to proj-vec)))
    (.set-rotation node angle))
  inst)
