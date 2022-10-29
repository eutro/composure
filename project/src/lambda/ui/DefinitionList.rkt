#lang gdlisp

(extends Control)

(define (export NodePath) list-path)
(define onready list-node (get-node list-path))
(define (export PackedScene) DefinitionEntry (preload "DefinitionEntry.tscn"))

(define (_ready)
  (.init-defs Game)
  (for ([entry Game.builtin-defs])
    (define node (.instance DefinitionEntry))
    (set! (.-editable node) false)
    (set! (.-entry node) entry)
    (.add-child list-node node))
  (for ([entry Game.user-defs])
    (define node (.instance DefinitionEntry))
    (set! (.-entry node) entry)
    (.add-child list-node node)))
