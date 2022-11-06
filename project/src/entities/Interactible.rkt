#lang gdlisp

(extends Spatial)

(define (export PackedScene) Panel)
(signal interacted)

(define onready panel $Panel)

(define (set-shape shape)
  (define cs (get-node "%CollisionShape"))
  (cs.set-transform shape.transform)
  (cs.set-shape cs.shape))

(define (_ready)
  (.hide panel))

(define (_on_InteractArea_area_entered area)
  (when (!= (.get-script area) (.get-script $InteractArea))
    (.show panel))
  null)

(define (_on_InteractArea_area_exited area)
  (when (!= (.get-script area) (.get-script $InteractArea))
    (.hide panel))
  null)
