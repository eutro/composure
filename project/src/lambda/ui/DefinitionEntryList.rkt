#lang gdlisp

(extends Control)

(define (export NodePath) list-path)
(define (export PackedScene) DefinitionEntry (preload "DefinitionEntry.tscn"))

(define (init entries)
  (define list-node (get-node list-path))
  (for ([entry entries])
    (define e-inst (.instance DefinitionEntry))
    (set! e-inst.entry entry)
    (.add-child list-node e-inst)))
