#lang gdlisp

(extends Spatial)

(define origin [0 0])
(define tiles {})

(define (export NodePath) player-path)
(define onready player (get-node player-path))

(define (_ready)
  (set! player.velocity (Vector3 1 0 0)))

(define (_enter-tree)
  (set! Game.world self))

(define (_exit-tree)
  (when (== Game.world self)
    (set! Game.world null))
  null)
