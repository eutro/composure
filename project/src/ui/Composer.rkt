#lang gdlisp

(extends Control)

(define (get-tabs)
  $HB/Tabs)

(define (get-definitions)
  $HB/DefinitionList)

(define (_enter-tree)
  (when (!= null Game.ui) (.play (.get-node Game.ui "Open"))))

(define (_exit-tree)
  (when (!= null Game.ui) (.play (.get-node Game.ui "Close"))))
