#lang gdlisp

(extends Spatial)

(class-name RandomTile)

(define (export NodePath) child-map)

(define (_ready)
  (randomize)
  (define cm (get-node child-map))
  (assert (cm . is . GridMap))
  (define used-cells (.get-used-cells cm))
  ;; this distribution is not exactly uniform and that deeply upsets me but we move
  (define i (% (randi) (len used-cells)))
  (define cpos (ref used-cells i))
  (define tile (.get-cell-item cm cpos.x cpos.y cpos.z))
  (define ori (.get-cell-item-orientation cm cpos.x cpos.y cpos.z))
  (.queue-free cm)

  (define gm (get-parent))
  (assert (gm . is . GridMap))
  (define pos (.world-to-map gm translation))
  (.set-cell-item gm pos.x pos.y pos.z tile ori)
  (queue-free)
  null)
