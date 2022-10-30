#lang gdlisp

(extends Spatial)

(define origin [0 0])
(define tiles {})

(define (export NodePath) player-path)
(define onready player (get-node player-path))

(define (export NodePath) camera-path)
(define onready camera (get-node camera-path))

(define (_enter-tree)
  (set! Game.world self))

(define (_exit-tree)
  (when (== Game.world self)
    (set! Game.world null))
  null)
