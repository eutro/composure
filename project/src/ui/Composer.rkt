#lang gdlisp

(extends Control)

(define (get-tabs)
  $HB/Tabs)

(define (get-definitions)
  $HB/DefinitionList)

(define (_enter-tree) (Game.play-open))

(define (_exit-tree) (Game.play-close))
