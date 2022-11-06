#lang gdlisp

(extends RichTextLabel)

(require threading)

(define (_ready)
  (define distance-offscreen
    (- (+ rect_global_position.x rect-size.x)
       (~> (get-viewport) .get-visible-rect .-size .-x (- 100))))
  (when (distance-offscreen . >= . 0)
    (-set! margin_left rect-size.x)
    (-set! margin_right rect-size.x))
  null)
