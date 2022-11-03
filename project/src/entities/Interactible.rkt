#lang gdlisp

(extends Spatial)

(define (export PackedScene) Panel)
(signal interacted)

(define (_ready)
  (define pnl (.instance Panel))
  (.add-child (.get-root $ViewportDisplay) pnl))
