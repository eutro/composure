#lang gdlisp

(extends Spatial)

(define (export NodePath) target-path)

(define SNAP_DISTANCE 0.1)

(define (_physics-process delta)
  (when (!= null target-path)
    (define target-pos
      (.-translation (get-node target-path)))
    (define dist-to-target
      (.distance-to translation target-pos))
    (cond
      [(dist-to-target . <= . SNAP_DISTANCE)
       (set! translation target-pos)]
      [else
       (define vel (/ (- target-pos translation) 0.5))
       (+set! translation (* vel (min delta 1)))])))
