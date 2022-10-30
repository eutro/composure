#lang gdlisp

(extends CanvasLayer)

(signal gui-changed #;showing)

(define (is-gui-showing)
  (.-mounted $Composer))

(define (_on_ComposerButton_pressed)
  (.toggle-mount $Composer)
  (emit-signal "gui_changed" (is-gui-showing)))
