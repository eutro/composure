#lang gdlisp

(extends RichTextLabel)

(require threading)

(define (_ready)
  (when (rect_global_position.x . >= . (~> (get-viewport) .get-size .-x (/ 2)))
    (set "margin_left" (- rect_size.x))))
