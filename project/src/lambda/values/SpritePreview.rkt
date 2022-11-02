#lang gdlisp

(class-name SpritePreview)
(extends Control)

(define static (create sprite)
  (define rect (.new TextureRect))
  (set! rect.texture sprite)
  (set! rect.expand true)
  (set! rect.rect-min-size (Vector2 50 50))
  rect)
