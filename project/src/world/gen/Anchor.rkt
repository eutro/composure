#lang gdlisp

tool
(extends Spatial)

(define (export int) type)

(define (get-direction)
  (define dir-3d
    (.rotated Vector3.FORWARD
              Vector3.UP
              global-rotation.y))
  (.normalized
   (Vector2 (int dir-3d.x) (int dir-3d.z))))
