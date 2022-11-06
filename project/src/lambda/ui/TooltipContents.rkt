#lang gdlisp

(extends RichTextLabel)

(require threading)

(define (_ready)
  (define distance-offscreen (- (+ rect_global_position.x rect-size.x) (~> (get-viewport) .get-visible-rect .-size .-x)))
  (when (distance-offscreen . >= . 0)
    (-set! margin_left distance-offscreen)
    (-set! margin_right distance-offscreen))
  null)
