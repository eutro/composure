#lang gdlisp

(extends HBoxContainer)

(define (export bool) editable true)

(define onready name-box $Name)
(define onready value $Value)

(define entry)

(signal entry-changed #;name #;value)

(define (_ready)
  (set! (.-editable name-box) editable)
  (set! (.-editable value) editable)
  (set! (.-remove-on-drag value) false)
  (when entry
    (.set-text name-box (ref entry 0))
    (.set-term value (ref entry 1)))
  (when (not editable)
    (.set name-box "focus_mode" FOCUS_NONE)
    (.set name-box "mouse_default_cursor_shape" CURSOR_ARROW))
  null)

(define (_on_Name_text_entered text)
  (.grab-focus value))

(define (_on_Name_text_changed text)
  (emit-signal "entry_changed" text value.term))

(define (_on_Value_term_changed term)
  (emit-signal "entry_changed" name-box.text term))
