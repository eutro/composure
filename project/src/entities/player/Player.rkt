#lang gdlisp

(extends KinematicBody)

(define onready at $AnimationTree)

(define MAX_SPEED 6)

(define RUN_THRESHOLD 4)

(define velocity : Vector3 (Vector3 0 0 0))

(define (unnan x)
  (if (is-nan x) 0 x))

(define (user-move vel last-vel)
  (+set! velocity
         (Vector3 (- (unnan vel.x) (unnan last-vel.x))
                  0
                  (- (unnan vel.y) (unnan last-vel.y))))
  (define len-sqr (.length-squared vel))
  (when (or (is-inf len-sqr)
            (is-nan len-sqr)
            (len-sqr . < . 0.1))
    (set! velocity (Vector3 0 0 0)))
  null)

(define (compare-distance a b)
  (< (global-translation.distance-squared-to a.global-translation)
     (global-translation.distance-squared-to b.global-translation)))

(define (interact-nearest)
  (define areas (.get-overlapping-areas $InteractionChecker))
  (when (not (.empty areas))
    (.sort-custom areas self "compare-distance")
    (define nearest-area (ref areas 0))
    (.interact nearest-area)))

(define (get-position)
  global-transform.origin)

(define (cap-vel vel)
  (define mag (.length vel))
  (if (> mag MAX_SPEED)
      (* vel (/ MAX_SPEED mag))
      vel))

(define (_physics-process delta)
  (define speed (.length velocity))
  (cond
    [(> speed 0)
     (define vel (cap-vel velocity))
     (look-at (+ translation vel) Vector3.UP)
     (set! speed (.length (move-and-slide vel)))
     (set! (ref at "parameters/Motion/blend_amount")
           (/ (min speed RUN_THRESHOLD) RUN_THRESHOLD))
     (set! (ref at "parameters/Run/blend_amount")
           (/ (max (- speed RUN_THRESHOLD) 0) (- MAX_SPEED RUN_THRESHOLD)))]
    [else
     (set! (ref at "parameters/Motion/blend_amount") 0)])
  null)
