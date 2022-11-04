#lang gdlisp

(extends Control)

(define (export PackedScene) scene)
(define (export NodePath) mount-point (#%gdscript "@\".\""))

(define mounted)

(define (mount)
  (when (== null mounted)
    (set! mounted (.instance scene))
    (.add-child (get-node mount-point) mounted))
  mounted)

(define (unmount)
  (when (!= null mounted)
    (.queue-free mounted)
    (set! mounted null)))

(define (toggle-mount)
  (if (== null mounted)
      (mount)
      (unmount))
  null)
