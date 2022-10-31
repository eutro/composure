#lang gdlisp

(extends Control)

(define (init key)
  (set! $Label.text (get-stick-name key.stick))
  (.show (.get-child $Cons key.stick))
  null)

(define (get-stick-name stick)
  (match stick
    [0 "Left Joystick Motion"]
    [1 "Right Joystick Motion"]))
