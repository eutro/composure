#lang gdlisp

(extends Control)

(define (init key)
  (set! $Label.text (get-button-name key.event.button-index))
  (.show (.get-child $Buttons (get-node-idx key.event.button-index)))
  null)

(define (get-node-idx button)
  (match button
    [BUTTON_LEFT 0]
    [BUTTON_RIGHT 1]
    [BUTTON_MIDDLE 2]
    [BUTTON_XBUTTON1 3]
    [BUTTON_XBUTTON2 4]
    [BUTTON_WHEEL_UP 2]
    [BUTTON_WHEEL_DOWN 2]
    [BUTTON_WHEEL_LEFT 2]
    [BUTTON_WHEEL_RIGHT 2]))

(define (get-button-name button)
  (match button
    [BUTTON_LEFT "Left Click"]
    [BUTTON_RIGHT "Right Click"]
    [BUTTON_MIDDLE "Middle Click"]
    [BUTTON_XBUTTON1 "Extra Button 1"]
    [BUTTON_XBUTTON2 "Extra Button 2"]
    [BUTTON_WHEEL_UP "Wheel Up"]
    [BUTTON_WHEEL_DOWN "Wheel Down"]
    [BUTTON_WHEEL_LEFT "Wheel Left"]
    [BUTTON_WHEEL_RIGHT "Wheel Right"]))
