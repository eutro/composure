#lang gdlisp

(extends Control)

(define (get-tabs)
  $HB/Tabs)

(define (_enter-tree)
  (.play (.get-node Game.ui "Open")))

(define (_exit-tree)
  (.play (.get-node Game.ui "Close")))
