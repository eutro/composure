#lang gdlisp

(class-name InputKeyMouse)
(require "../../macros.rkt")

(extends InputKeyBase)

(splice-record ([event : InputEventMouseButton]))

(define (_map-key)
  ["Mouse" (.-button-index event)])

(define (_scene)
  (preload "MouseDisplay.tscn"))

(define (_lambda-type)
  Types.TY_VEC2)

(define (get-lock-type)
  (.-pressed event))

(define static (normalise-pos pos)
  (define vpr (.get-visible-rect (Game.get-viewport)))
  (* 2 (- (/ pos vpr.size) (Vector2 0.5 0.5))))

(define static (denormalise-pos pos)
  (define vpr (.get-visible-rect (Game.get-viewport)))
  (* (+ (/ pos 2) (Vector2 0.5 0.5)) vpr.size))

(define (resolve-value)
  (Values.wrap-vec2 (normalise-pos (.get-mouse-position (Game.get-viewport)))))
