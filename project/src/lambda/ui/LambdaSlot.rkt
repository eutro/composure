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
  (when (and editable (not (Input.is-action-pressed "ui_duplicate")))
    (set-term null))
  t)

(define (can-drop-data _posn data)
  (and editable (is data LambdaValue)))

(define (drop-data _posn data)
  (set-term data))

(define (_gui-input evt)
  (cond
    [(.is-action evt "ui_pick")
     (Game.set-copy-source (get-path))
     (.show $Selected)
     (.connect Game "copy_source_changed" self "_on_Game_deselect")
     (.connect self "tree_exited" self "_on_tree_exited")
     (accept-event)]
    [(and editable (.is-action evt "ui_drop"))
     (when (!= null Game.copy-src)
       (define node (get-node-or-null Game.copy-src))
       (when (!= null node)
         (set-term (.-term node))
         (accept-event)))]
    [(and editable (.is-action evt "ui_delete"))
     (set-term null)
     (accept-event)])
  null)

(define (_on_Game_deselect)
  (.hide $Selected)
  (.disconnect Game "copy_source_changed" self "_on_Game_deselect")
  (.disconnect self "tree_exited" self "_on_tree_exited"))

(define (_on_tree_exited)
  (Game.set-copy-source null))

(define (_on-LambdaSlot-focus-entered)
  (.show $Focus))

(define (_on-LambdaSlot-focus-exited)
  (.hide $Focus))
