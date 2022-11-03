#lang gdlisp

(extends Spatial)

(define (export PackedScene) Panel)
(signal interacted)

(define onready panel $Panel)

(define (_ready)
  (.hide panel))

(define (_on_InteractArea_area_entered area)
  (when (!= (.get-script area) (get-script))
    (.show panel)))

(define (_on_InteractArea_area_exited area)
  (when (!= (.get-script area) (get-script))
    (.hide panel)))
