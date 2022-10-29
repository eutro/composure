#lang gdlisp

(extends Node)

#;#;
(class HiPrinter
  (define (run)
    (print "Hello!!")
    null))

(define (_ready)
  (define evt (InputEventKey.new))
  (set! (.-scancode evt) KEY_SPACE)
  (.bind-key Game.keys evt (.new HiPrinter))
  null)

(define (_unhandled-input evt)
  (define key (InputKeys.evt->input-key evt))
  (define binding (.lookup-key Game.keys evt))
  (when (!= null binding)
    (.apply binding (.resolve-value key)))
  null)
