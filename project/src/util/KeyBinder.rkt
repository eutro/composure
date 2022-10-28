#lang gdlisp

(class-name KeyBinder)

(define key-map {})

(class InputKey
  (define (_map-key)
    null))

(class InputKeyKey (extends InputKey)
  (define evt : InputEventKey)
  (define (_init evt)
    (set! (.-evt self) evt))
  (define (_map-key)
    ["Key" (.get-scancode-with-modifiers evt)]))

(class InputKeyMouse (extends InputKey)
  (define evt : InputEventMouse)
  (define (_init evt)
    (set! (.-evt self) evt))
  (define (_map-key)
    ["Mouse" (.button-mask evt)]))

(define (evt->input-key [evt : InputEvent])
  (print evt)
  (match (.get-class evt)
    ["InputEventKey" (.new InputKeyKey evt)]
    ["InputEventMouse" (.new InputKeyMouse evt)]
    [_ null]))

(define (bind-key [evt : InputEvent] binding)
  (define key (evt->input-key evt))
  (cond
    [(== key null)
     null]
    [(== binding null)
     (.erase (._map-key key-map) key)]
    [else
     (set! (ref key-map (._map-key key)) binding)])
  null)

(define (lookup-key [evt : InputEvent])
  (define key (evt->input-key evt))
  (if (== key null)
      null
      (.get key-map (._map-key key) null)))
