#lang gdlisp

(extends Node)

(define (export Array String) animations [])
(define (export NodePath) player-path)

(define (_ready)
  (define player (get-node player-path))
  (for ([anim animations])
    (.set-loop (.get-animation player anim) true)))
