#lang gdlisp

(extends HSlider)

(define (export int) bus)

(define (_ready)
  (set! value (db2linear (AudioServer.get-bus-volume-db bus))))

(define (_on_BusSlider_drag_ended vc)
  (when vc (Game.save-volume)))

(define (_on_BusSlider_value_changed vl)
  (AudioServer.set-bus-volume-db bus (linear2db vl)))
