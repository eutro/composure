#lang gdlisp

tool
(class-name CRoom)
(extends Resource)
(require "../../macros.rkt")

;; all the edges which are identified as walls
(define (export Array) edges : Array #;[#;pos Vector2])
;; the anchors which we can place this by
(define (export Array) anchors : Array #;[#;pos Vector2 #;type int #;direction Vector2])

;; the rest of the tiles in the room
(define (export Dictionary) tiles : Dictionary #;{Vector3 [#;item int #;ori int]})
;; the rest of the objects in this room
(define (export PackedScene) entities : PackedScene)
