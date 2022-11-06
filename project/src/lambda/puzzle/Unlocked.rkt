#lang gdlisp

(extends Control)
(require "../../macros.rkt" threading)

(define const LambdaSlot (preload "res://src/lambda/ui/LambdaSlot.tscn"))

(define (init unlocked)
  (define gd $PN/SC/VB/GC)
  (for ([e unlocked])
    (doto gd
     (.add-child (doto (.new Label) (.set-text (ref e 0))))
     (.add-child (doto (.new Label) (.set-text (ref (ref e 1) 0))))
     (.add-child (doto (.instance LambdaSlot)
                  (.ignore-next-sound)
                  (.set-term (ref (ref e 1) 1))
                  (~> .-editable (set! false))))))
  null)

(define (_on_Close_pressed)
  (.unmount (get-parent)))

(define (_enter-tree) (Game.play-open))

(define (_exit-tree) (Game.play-close))
