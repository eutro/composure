#lang gdlisp

(extends Control)

(define (export NodePath) initial-focus null)

(define (_ready)
  (when (!= null initial-focus)
    (.call-deferred (get-node initial-focus) "grab_focus"))
  null)
