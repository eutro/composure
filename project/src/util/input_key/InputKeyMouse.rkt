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

(define (get-lock-type)
  (.-pressed event))

(define (resolve-value)
  (define pos-2d (.get-mouse-position (Game.get-viewport)))
  (define drop-plane (Plane Vector3.UP 0))
  (define pos-3d
    (.intersects-ray
     drop-plane
     (.project-ray-origin Game.world.camera pos-2d)
     (.project-ray-normal Game.world.camera pos-2d)))

  (Values.wrap-vec
   (if (== null pos-3d)
       (Vector3 0 0 0)
       (- pos-3d) ;; TODO relative to player
       )))
