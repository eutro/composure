#lang gdlisp

(class-name LambdaSlot)
(extends Control)

(define term : LambdaValue)

(define (export bool) editable true)

(signal term_changed #;term)

(define preview null)

(define (set-term new-term)
  (when (!= null preview)
    (remove-child preview)
    (.queue-free preview)
    (set! preview null)
    (set! hint_tooltip ""))

  (when (!= null new-term)
    (set! preview (.create-preview new-term))
    (add-child preview)
    (set! hint_tooltip (.to-string (.get-type new-term))))

  (set! term new-term)
  (emit-signal "term_changed" term))

(define (get-drag-data _posn)
  (when (!= null term)
    (define cont (.new CenterContainer))
    (set! (.-use-top-left cont) true)
    (.add-child cont (.create-preview term))
    (set-drag-preview cont))
  (define t term)
  (when editable
    (set-term null))
  t)

(define (can-drop-data _posn data)
  (and editable (is data LambdaValue)))

(define (drop-data _posn data)
  (set-term data))
