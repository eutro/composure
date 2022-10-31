#lang gdlisp

(class-name InputKeys)

(define static (evt->input-key evt)
  (match (.get-class evt)
    ["InputEventKey" (InputKeyKeyboard.new evt)]
    ["InputEventMouseButton" (InputKeyMouse.new evt)]
    [_ null]))

(begin-escape
  (define-syntax-rule (axes->joyinput stick left right up down)
    (when (or (Input.is-action-just-pressed left true)
              (Input.is-action-just-pressed right true)
              (Input.is-action-just-pressed up true)
              (Input.is-action-just-pressed down true))
      (.append actions (InputKeyJoystick.new stick left right up down)))))

(define static (collect-grouped)
  (define actions [])
  (axes->joyinput 0 "joy_left_left" "joy_left_right" "joy_left_up" "joy_left_down")
  (axes->joyinput 1 "joy_right_left" "joy_right_right" "joy_right_up" "joy_right_down")
  actions)
