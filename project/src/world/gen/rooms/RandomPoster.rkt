#lang gdlisp

(extends Spatial)

(define (_ready)
  (.set-texture $Sprite3D (PosterRng.next)))
