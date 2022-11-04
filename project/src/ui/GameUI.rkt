#lang gdlisp

(extends CanvasLayer)

(signal gui-changed #;showing)

(define (export PackedScene) Printer)

(define (get-composer-button)
  $HUD/ComposerButton)

(define (_ready)
  (set! Game.ui self)
  (.start (.mount $Tutorial)))

(define (get-gui)
  (.-mounted $Composer))

(define (gui-changed)
  (emit-signal "gui-changed" (is-gui-showing)))

(define (mount-gui)
  (.mount $Composer)
  (emit-signal "gui_changed" true)
  (get-gui))

(define (unmount-gui)
  (.unmount $Composer)
  (emit-signal "gui_changed" false))

(define (is-gui-showing)
  (!= null (get-gui)))

(define (_on_ComposerButton_pressed)
  (.toggle-mount $Composer)
  (emit-signal "gui_changed" (is-gui-showing)))

(define (add-printer)
  (define printer (.instance Printer))
  (.add-child $HUD/Printers printer)
  printer)
