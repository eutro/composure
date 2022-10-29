#lang gdlisp

(class-name TextPreview)
(extends Control)

(define (set-text text)
  (set! (.-text $Label) text))

(define (get-text)
  (.-text $Label))

(define static (create text)
  (define node (.instance (load "res://src/lambda/ui/TextPreview.tscn")))
  (.set-text node text)
  node)
