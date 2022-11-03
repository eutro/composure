#lang gdlisp

(extends Control)

(define (export NodePath) initial-focus null)

(define (_ready)
  (when (!= null initial-focus)
    (define initial (get-node initial-focus))
    (.ignore-next-sound initial)
    (.call-deferred initial "grab_focus"))
  null)
