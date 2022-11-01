#lang gdlisp

(extends CanvasLayer)

(signal gui-changed #;showing)

(define (export PackedScene) Printer)

(define (_ready)
  (set! Game.ui self))

(define (is-gui-showing)
  (!= null (.-mounted $Composer)))

(define (_on_ComposerButton_pressed)
  (.toggle-mount $Composer)
  (emit-signal "gui_changed" (is-gui-showing)))

(define (add-printer)
  (define printer (.instance Printer))
  (.add-child $HUD/Printers printer)
  printer)
