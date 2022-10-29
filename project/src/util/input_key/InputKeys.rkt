#lang gdlisp

(class-name InputKeys)

(define static (evt->input-key evt)
  (match (.get-class evt)
    ["InputEventKey" (.new InputKeyKeyboard evt)]
    ["InputEventMouseButton" (.new InputKeyMouse evt)]
    [_ null]))
