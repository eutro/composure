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
  Types.TY_VEC2)

(define (get-lock-type)
  (.-pressed event))

(define static (normalise-pos pos)
  (define vp (Game.get-viewport))
  (* 2 (- (/ pos vp.size) (Vector2 0.5 0.5))))

(define static (denormalise-pos pos)
  (define vp (Game.get-viewport))
  (* (+ (/ pos 2) (Vector2 0.5 0.5)) vp.size))

(define (resolve-value)
  (Values.wrap-vec2 (normalise-pos (.get-mouse-position (Game.get-viewport)))))
