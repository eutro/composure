#lang gdlisp

(extends Control)

(signal close-pressed)

(define (_on_Close_pressed)
  (emit-signal "close_pressed"))
