#lang gdlisp

(class-name InputKeyJoystick)
(require "../../macros.rkt")

(extends InputKeyBase)

(splice-record ([stick : int]
                [left : String] [right : String]
                [up : String]  [down : String]))

(define (_map-key)
  ["Joystick" stick])

(define (_scene)
  (preload "JoystickDisplay.tscn"))

(define (get-lock-type)
  ((.length-squared (.-value (resolve-value))) . > . 0))

(define (_lambda-type)
  Types.TY_VEC2)

(define (resolve-value)
  (Values.wrap-vec2
   (Input.get-vector left right up down)))
