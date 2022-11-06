#lang gdlisp

(extends Node)

(define (_enter-tree)
  (Game.load-data))

(define (_ready)
  (Game.maybe-start-tutorial))

(define (_on_GameUI_gui_changed showing)
  (set! $BindingRunner.active (not showing)))
