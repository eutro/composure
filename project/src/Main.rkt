#lang gdlisp

(extends Node)

(define (_on_GameUI_gui_changed showing)
  (set! $BindingRunner.active (not showing)))
