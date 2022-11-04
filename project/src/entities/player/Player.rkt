#lang gdlisp

(extends KinematicBody)

(define onready at $AnimationTree)

(define const ACCEL_SPEED 10)
(define const MAX_SPEED 6)
(define const RUN_THRESHOLD 4)

(define target-vel : Vector3 (Vector3 0 0 0))
(define velocity : Vector3 (Vector3 0 0 0))

(define (unnan x)
  (if (is-nan x) 0 x))

(define (user-move vel last-vel)
  (+set! target-vel
         (Vector3 (- (unnan vel.x) (unnan last-vel.x))
                  0
                  (- (unnan vel.y) (unnan last-vel.y))))
  (define len-sqr (.length-squared vel))
  (when (or (is-inf len-sqr)
            (is-nan len-sqr)
            (len-sqr . < . 0.1))
    (set! target-vel (Vector3 0 0 0)))
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
  (cond
    [(== target-vel Vector3.ZERO) null]
    [else (set! velocity (* (.length velocity) (.normalized target-vel)))])
  (fset! velocity .move-toward target-vel (* delta ACCEL_SPEED))
  (fset! velocity cap-vel)

  (cond
    [((.length velocity) . > . 0)
     (when (!= target-vel Vector3.ZERO)
       (look-at (+ translation target-vel) Vector3.UP))
     (fset! velocity move-and-slide)
     (set! translation.y 0) ;; So we can't run up chairs and stay there -_-
     (define speed (.length velocity))
     (set! (ref at "parameters/Motion/blend_amount")
           (/ (min speed RUN_THRESHOLD) RUN_THRESHOLD))
     (set! (ref at "parameters/Run/blend_amount")
           (/ (max (- speed RUN_THRESHOLD) 0) (- MAX_SPEED RUN_THRESHOLD)))]
    [else
     (set! (ref at "parameters/Motion/blend_amount") 0)])
  null)
